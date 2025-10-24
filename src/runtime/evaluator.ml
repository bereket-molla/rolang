(* evaluator.ml - executes expressions in topological order
   
   implements forward cone scheduling - only recompute what changed
*)

open Ast
open Value
open Graph

(* evaluation environment *)
type eval_env = {
  registry: Signal_registry.registry;
  stream_registry: Stream.stream_registry;
  window_registry: Temporal.window_registry;
  sample_registry: Temporal.sample_registry;
  graph: Graph.graph;
  topo_order: int list;
}

let create_env dependency_graph topological_order = {
  registry = Signal_registry.create ();
  stream_registry = Stream.create_stream_registry ();
  window_registry = Temporal.create_window_registry ();
  sample_registry = Temporal.create_sample_registry ();
  graph = dependency_graph;
  topo_order = topological_order;
}


(* expression evaluation *)
let rec eval environment expr =
  match expr with
  | ELit lit_value -> value_of_literal lit_value
  
  | EVar variable_name ->
      Signal_registry.get environment.registry variable_name
  
  | EUnary (operator, operand_expr) ->
      let operand_value = eval environment operand_expr in
      (match operator with
       | Neg -> VInt (- (as_int operand_value))
       | FNeg -> VFloat (-. (as_float operand_value))
       | Not -> VBool (not (as_bool operand_value)))
  
  | EBinary (binary_op, left_expr, right_expr) ->
      eval_binary environment binary_op left_expr right_expr
  
  | ETemporal (temporal_op, operand_expr) ->
      eval_temporal environment temporal_op operand_expr
  
  | EApply (function_expr, argument_expr) ->
      let function_value = eval environment function_expr in
      let argument_value = eval environment argument_expr in
      (match function_value with
       | VClosure (parameters, body_expr, closure_env) ->
           (match parameters with
            | [PVar param_name] ->
                let extended_env = (param_name, argument_value) :: closure_env in
                eval_with_locals environment extended_env body_expr
            | PVar first_param :: remaining_params ->
                (* partial application *)
                let extended_env = (first_param, argument_value) :: closure_env in
                VClosure (remaining_params, body_expr, extended_env)
            | _ -> failwith "Pattern parameters not yet supported in application")
       | _ -> failwith "Cannot apply non-function")
  
  | ELambda (lambda_params, lambda_body) ->
      VClosure (lambda_params, lambda_body, [])
  
  | EIf (condition_expr, then_expr, else_expr) ->
      if as_bool (eval environment condition_expr) then
        eval environment then_expr
      else
        eval environment else_expr
  
  | ELet (binding_name, _, bound_expr, body_expr) ->
      let bound_value = eval environment bound_expr in
      let local_bindings = [(binding_name, bound_value)] in
      eval_with_locals environment local_bindings body_expr
  
  | ELetRec (recursive_name, _, recursive_expr, body_expr) ->
      (* simplified - assume recursive_expr is a lambda *)
      let recursive_value = eval environment recursive_expr in
      let local_bindings = [(recursive_name, recursive_value)] in
      eval_with_locals environment local_bindings body_expr
  
  | ETuple tuple_elements ->
      VTuple (List.map (eval environment) tuple_elements)
  
  | ERecord record_fields ->
      VRecord (List.map (fun (field_name, field_expr) -> (field_name, eval environment field_expr)) record_fields)
  
  | EField (record_expr, field_name) ->
      (match eval environment record_expr with
       | VRecord field_values ->
           (match List.assoc_opt field_name field_values with
            | Some field_value -> field_value
            | None -> failwith (Printf.sprintf "No field '%s'" field_name))
       | _ -> failwith "Field access on non-record")
  
  | EList list_elements ->
      VList (List.map (eval environment) list_elements)
  
  | EListCons (head_expr, tail_expr) ->
      let head_value = eval environment head_expr in
      (match eval environment tail_expr with
       | VList tail_values -> VList (head_value :: tail_values)
       | _ -> failwith ":: requires list")
  
  | EMatch (scrutinee_expr, match_cases) ->
      let scrutinee_value = eval environment scrutinee_expr in
      eval_match environment scrutinee_value match_cases
  
  | EBlock (_, final_expr) ->
      (* simplified - ignore statements for now *)
      eval environment final_expr
  
  | EStream _ ->
      (* streams don't evaluate directly *)
      VUnit
  
  | ESampleEvery (sampled_expr, stream_name) ->
      let current_value = eval environment sampled_expr in
      (* check if stream fired this index *)
      let did_stream_fire = match Stream.find_stream environment.stream_registry stream_name with
        | Some stream_state -> stream_state.last_fired_index = environment.registry.current_index
        | None -> false
      in
      Temporal.sample_value environment.sample_registry 
        (match sampled_expr with EVar n -> n | _ -> "_") 
        stream_name current_value environment.registry.current_index did_stream_fire

and eval_with_locals environment local_bindings expr_to_eval =
  (* FIXME: this temporarily adds to registry which isn't great *)
  List.iter (fun (var_name, var_value) ->
    if not (Hashtbl.mem environment.registry.entries var_name) then
      Signal_registry.register environment.registry var_name var_value EqExact
    else
      Signal_registry.set environment.registry var_name var_value
  ) local_bindings;
  eval environment expr_to_eval

and eval_binary environment binary_operator left_expr right_expr =
  let left_value = eval environment left_expr in
  let right_value = eval environment right_expr in
  match binary_operator with
  (* int arithmetic *)
  | Add -> VInt (as_int left_value + as_int right_value)
  | Sub -> VInt (as_int left_value - as_int right_value)
  | Mul -> VInt (as_int left_value * as_int right_value)
  | Div -> VInt (as_int left_value / as_int right_value)
  | Mod -> VInt (as_int left_value mod as_int right_value)
  
  (* float arithmetic *)
  | FAdd -> VFloat (as_float left_value +. as_float right_value)
  | FSub -> VFloat (as_float left_value -. as_float right_value)
  | FMul -> VFloat (as_float left_value *. as_float right_value)
  | FDiv -> VFloat (as_float left_value /. as_float right_value)
  
  (* comparison *)
  | Eq -> VBool (values_equal left_value right_value)
  | Neq -> VBool (not (values_equal left_value right_value))
  | Lt -> VBool (compare_values left_value right_value < 0)
  | Gt -> VBool (compare_values left_value right_value > 0)
  | Le -> VBool (compare_values left_value right_value <= 0)
  | Ge -> VBool (compare_values left_value right_value >= 0)
  
  (* logical *)
  | And -> VBool (as_bool left_value && as_bool right_value)
  | Or -> VBool (as_bool left_value || as_bool right_value)
  
  (* pipe operator *)
  | Pipe ->
      (match right_value with
       | VClosure (params, body, closure_env) ->
           (match params with
            | [PVar param_name] ->
                let extended_env = (param_name, left_value) :: closure_env in
                eval_with_locals environment extended_env body
            | _ -> failwith "Multi-param pipe not yet supported")
       | _ -> failwith "Pipe requires function")
  
  | Compose ->
      (* TODO: proper function composition *)
      (match left_value, right_value with
       | VClosure _, VClosure _ ->
           VClosure ([PVar "x"], 
                    EApply (ELit (LInt 0), ELit (LInt 0)),
                    [])
       | _ -> failwith "Composition requires functions")

and compare_values val1 val2 =
  match val1, val2 with
  | VInt n1, VInt n2 -> compare n1 n2
  | VFloat f1, VFloat f2 -> compare f1 f2
  | VString s1, VString s2 -> compare s1 s2
  | VChar c1, VChar c2 -> compare c1 c2
  | VBool b1, VBool b2 -> compare b1 b2
  | _ -> failwith "Incomparable values"

and eval_temporal environment temporal_operator operand_expr =
  match temporal_operator with
  | Prev ->
      (match operand_expr with
       | EVar signal_name -> Signal_registry.get_prev environment.registry signal_name
       | _ -> failwith "prev requires a variable")
  
  | Window window_size ->
      (match operand_expr with
       | EVar signal_name ->
           let current_value = Signal_registry.get environment.registry signal_name in
           Temporal.update_window environment.window_registry signal_name window_size current_value
       | _ -> failwith "window requires a variable")
  
  | Sample ->
      eval environment operand_expr

and eval_match environment scrutinee_value match_cases =
  let rec try_case = function
    | [] -> failwith "Match: no pattern matched"
    | (case_pattern, guard_expr_opt, case_body) :: remaining_cases ->
        match try_pattern environment case_pattern scrutinee_value with
        | None -> try_case remaining_cases
        | Some pattern_bindings ->
            (* check guard if present *)
            let guard_passes = match guard_expr_opt with
              | None -> true
              | Some guard_expr ->
                  let guard_value = eval_with_locals environment pattern_bindings guard_expr in
                  as_bool guard_value
            in
            if guard_passes then
              eval_with_locals environment pattern_bindings case_body
            else
              try_case remaining_cases
  in
  try_case match_cases

and try_pattern environment pattern_to_match value_to_match =
  match pattern_to_match, value_to_match with
  | PVar var_name, v -> Some [(var_name, v)]
  | PWild, _ -> Some []
  | PLit pattern_lit, v -> 
      if values_equal (value_of_literal pattern_lit) v then Some [] else None
  | PTuple pattern_elements, VTuple value_elements when List.length pattern_elements = List.length value_elements ->
      (try
        let all_bindings = List.concat (List.map2 (fun pat val_elem ->
          match try_pattern environment pat val_elem with
          | Some bindings -> bindings
          | None -> raise Not_found
        ) pattern_elements value_elements) in
        Some all_bindings
       with Not_found -> None)
  | PVariant (ctor, None), VVariant (c, None) when ctor = c ->
      Some []
  | PVariant (ctor, Some arg_pat), VVariant (c, Some arg_val) when ctor = c ->
      try_pattern environment arg_pat arg_val
  | PVariant ("[]", None), VList [] ->
      Some []
  | PVariant ("::", Some (PTuple [head_pat; tail_pat])), VList (head_val :: tail_vals) ->
      (match try_pattern environment head_pat head_val with
       | None -> None
       | Some head_bindings ->
           match try_pattern environment tail_pat (VList tail_vals) with
           | None -> None
           | Some tail_bindings -> Some (head_bindings @ tail_bindings))
  | _ -> None


(* evaluate a single node *)
let eval_node environment graph_node =
  match graph_node.kind, graph_node.expr with
  | NSignal _, _ ->
      ()  (* signals populated externally *)
  
  | NLet (let_name, _), Some let_expr ->
      let evaluated_value = eval environment let_expr in
      Signal_registry.set environment.registry let_name evaluated_value
  
  | NCell (cell_name, _), Some cell_expr ->
      let evaluated_value = eval environment cell_expr in
      Signal_registry.set environment.registry cell_name evaluated_value
  
  | NStream (stream_name, stream_def), _ ->
      (* execute stream if it should fire *)
      (match Stream.find_stream environment.stream_registry stream_name with
       | Some stream_state ->
           (match Stream.execute_stream stream_state (eval environment) environment.registry.current_index with
            | Some emitted_value ->
                Signal_registry.set environment.registry stream_name emitted_value
            | None -> ())
       | None -> failwith (Printf.sprintf "Stream '%s' not found" stream_name))
  
  | _, None ->
      ()
  
  | NExpr _, Some anonymous_expr ->
      let _ = eval environment anonymous_expr in
      ()


(* evaluate all nodes in topological order *)
let eval_full_sweep environment =
  List.iter (fun node_id ->
    match get_node environment.graph node_id with
    | Some graph_node -> eval_node environment graph_node
    | None -> ()
  ) environment.topo_order

(* evaluate only forward cone *)
let eval_forward_cone environment changed_source_names =
  (* get source IDs *)
  let source_ids = List.filter_map (fun source_name ->
    Hashtbl.find_opt environment.graph.name_to_id source_name
  ) changed_source_names in
  
  (* compute forward cone *)
  let cone_node_ids = Forward_cone.forward_cone environment.graph source_ids in
  
  (* filter topological order to only cone nodes *)
  let cone_set = List.fold_left (fun hash_acc node_id ->
    Hashtbl.add hash_acc node_id (); hash_acc
  ) (Hashtbl.create (List.length cone_node_ids)) cone_node_ids in
  
  let nodes_in_cone_order = List.filter (fun node_id ->
    Hashtbl.mem cone_set node_id
  ) environment.topo_order in
  
  (* evaluate in order *)
  List.iter (fun node_id ->
    match get_node environment.graph node_id with
    | Some graph_node -> eval_node environment graph_node
    | None -> ()
  ) nodes_in_cone_order;
  
  Printf.printf "Evaluated %d / %d nodes (%.1f%%)\n"
    (List.length nodes_in_cone_order)
    environment.graph.node_count
    ((float_of_int (List.length nodes_in_cone_order)) /. 
     (float_of_int environment.graph.node_count) *. 100.0)


(* initialize runtime from graph *)
let initialize_from_graph environment =
  (* register all signals *)
  Array.iter (fun graph_node ->
    match graph_node.kind with
    | NSignal (signal_name, signal_type) ->
        Signal_registry.register_with_default environment.registry signal_name signal_type
    | NLet (let_name, let_type) ->
        Signal_registry.register_with_default environment.registry let_name let_type
    | NCell (cell_name, cell_type) ->
        Signal_registry.register_with_default environment.registry cell_name cell_type
    | NStream (stream_name, stream_definition) ->
        (* register stream and output signal *)
        let cadence_milliseconds = None in  (* TODO: parse from time declarations *)
        let stream_state = Stream.register_stream environment.stream_registry stream_name stream_definition cadence_milliseconds in
        Stream.initialize_stream stream_state (eval environment);
        (* register output signal *)
        Signal_registry.register_with_default environment.registry stream_name (TyBase TInt)
    | NExpr _ -> ()
  ) environment.graph.nodes;
  
  (* initial evaluation *)
  eval_full_sweep environment
