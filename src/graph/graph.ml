(* graph.ml - builds the dependency DAG from the AST
   
   this is how we figure out what depends on what so we can
   do the forward cone optimization
*)

open Ast

(* graph node representation *)

type node_id = int

type node_kind =
  | NSignal of string * ty  (* external input *)
  | NLet of string * ty  (* pure expression *)
  | NCell of string * ty  (* mutable cell *)
  | NStream of string * stream_def  (* periodic block *)
  | NExpr of expr  (* anonymous expression *)

type node = {
  id: node_id;
  kind: node_kind;
  expr: expr option;  (* the expression this node computes *)
  deps: node_id list;  (* list of node ids this depends on *)
}

type graph = {
  nodes: node array;  (* all nodes by ID *)
  node_count: int;
  name_to_id: (string, node_id) Hashtbl.t;  (* lookup table *)
  sources: node_id list;  (* signals and streams *)
}


(* dependency extraction - find all free variables in an expression *)

module NameSet = Set.Make(String)

let rec free_vars expr =
  match expr with
  | ELit _ -> NameSet.empty
  | EVar variable_name -> NameSet.singleton variable_name
  
  | EUnary (_, operand) -> free_vars operand
  | EBinary (_, left_expr, right_expr) -> 
      NameSet.union (free_vars left_expr) (free_vars right_expr)
  | ETemporal (_, inner_expr) -> free_vars inner_expr
  
  | EApply (func_expr, arg_expr) -> 
      NameSet.union (free_vars func_expr) (free_vars arg_expr)
  
  | ELambda (params, body) ->
      let bound_names = pattern_vars_list params in
      NameSet.diff (free_vars body) bound_names
  
  | EIf (cond, then_branch, else_branch) ->
      NameSet.union (free_vars cond) 
        (NameSet.union (free_vars then_branch) (free_vars else_branch))
  
  | ELet (bound_name, _, bound_expr, body_expr) ->
      NameSet.union (free_vars bound_expr)
        (NameSet.remove bound_name (free_vars body_expr))
  
  | ELetRec (func_name, _, func_expr, body_expr) ->
      NameSet.union (NameSet.remove func_name (free_vars func_expr))
        (NameSet.remove func_name (free_vars body_expr))
  
  | ETuple elements ->
      List.fold_left NameSet.union NameSet.empty (List.map free_vars elements)
  
  | ERecord fields ->
      List.fold_left
        (fun accumulated_vars (_, field_expr) -> NameSet.union accumulated_vars (free_vars field_expr))
        NameSet.empty fields
  
  | EField (record_expr, _) -> free_vars record_expr
  
  | EList list_elements ->
      List.fold_left NameSet.union NameSet.empty (List.map free_vars list_elements)
  
  | EListCons (head_expr, tail_expr) -> 
      NameSet.union (free_vars head_expr) (free_vars tail_expr)
  
  | EMatch (scrutinee, cases) ->
      let scrutinee_vars = free_vars scrutinee in
      List.fold_left
        (fun accumulated_vars (case_pattern, guard_opt, case_body) ->
          let pattern_bound = pattern_vars case_pattern in
          let guard_variables = match guard_opt with
            | None -> NameSet.empty
            | Some guard_expr -> NameSet.diff (free_vars guard_expr) pattern_bound
          in
          let body_variables = NameSet.diff (free_vars case_body) pattern_bound in
          NameSet.union accumulated_vars (NameSet.union guard_variables body_variables))
        scrutinee_vars cases
  
  | EBlock (_stmts, final_expr) ->
      (* TODO: handle statement scoping properly *)
      free_vars final_expr
  
  | EStream stream_def ->
      stream_free_vars stream_def
  
  | ESampleEvery (sampled_expr, _) -> free_vars sampled_expr

and pattern_vars pat =
  match pat with
  | PVar var_name -> NameSet.singleton var_name
  | PWild -> NameSet.empty
  | PLit _ -> NameSet.empty
  | PTuple patterns -> pattern_vars_list patterns
  | PRecord field_patterns ->
      List.fold_left
        (fun accumulated (_, field_pattern) -> NameSet.union accumulated (pattern_vars field_pattern))
        NameSet.empty field_patterns
  | PVariant (_, Some arg_pattern) -> pattern_vars arg_pattern
  | PVariant (_, None) -> NameSet.empty
  | PAs (inner_pat, alias_name) -> NameSet.add alias_name (pattern_vars inner_pat)
  | POr (pat1, pat2) -> NameSet.union (pattern_vars pat1) (pattern_vars pat2)

and pattern_vars_list patterns =
  List.fold_left NameSet.union NameSet.empty (List.map pattern_vars patterns)

and stream_free_vars stream_definition =
  (* stream depends on all vars used in updates and emit *)
  let state_variables = pattern_vars_list (List.map fst stream_definition.stream_state) in
  let init_variables = 
    List.fold_left NameSet.union NameSet.empty
      (List.map (fun (_, init_expr) -> free_vars init_expr) stream_definition.stream_state)
  in
  let update_variables =
    List.fold_left NameSet.union NameSet.empty
      (List.map (fun (_, upd_expr) -> 
        NameSet.diff (free_vars upd_expr) state_variables) stream_definition.stream_updates)
  in
  let emit_variables = match stream_definition.stream_emit with
    | None -> NameSet.empty
    | Some emit_expr -> NameSet.diff (free_vars emit_expr) state_variables
  in
  NameSet.union init_variables (NameSet.union update_variables emit_variables)


(* graph construction from AST *)

let build_graph program_stmts =
  let next_node_id = ref 0 in
  let get_id () = 
    let current_id = !next_node_id in 
    incr next_node_id; 
    current_id 
  in
  
  let name_lookup_table = Hashtbl.create 128 in
  let constructed_nodes = ref [] in
  let source_node_ids = ref [] in
  
  (* helper to add a node and return its ID *)
  let add_node node_kind expr_opt dependency_ids =
    let new_id = get_id () in
    let new_node = { id = new_id; kind = node_kind; expr = expr_opt; deps = dependency_ids } in
    constructed_nodes := new_node :: !constructed_nodes;
    new_id
  in
  
  (* resolve name dependencies to actual node IDs *)
  let resolve_deps variable_names =
    NameSet.fold
      (fun var_name accumulated_ids ->
        match Hashtbl.find_opt name_lookup_table var_name with
        | Some node_id -> node_id :: accumulated_ids
        | None -> accumulated_ids)  (* ignore unbound - probably builtins *)
      variable_names []
  in
  
  (* process each statement and build nodes *)
  let process_stmt stmt =
    match stmt with
    | SLet (let_name, type_opt, let_expr) ->
        let let_type = match type_opt with Some t -> t | None -> TyBase TUnit in
        let dependency_list = resolve_deps (free_vars let_expr) in
        let new_id = add_node (NLet (let_name, let_type)) (Some let_expr) dependency_list in
        Hashtbl.add name_lookup_table let_name new_id
    
    | SLetRec (rec_name, type_opt, rec_expr) ->
        let rec_type = match type_opt with Some t -> t | None -> TyBase TUnit in
        (* add node first to allow self-reference *)
        let new_id = get_id () in
        Hashtbl.add name_lookup_table rec_name new_id;
        let dependency_list = resolve_deps (free_vars rec_expr) in
        let recursive_node = { id = new_id; kind = NLet (rec_name, rec_type); expr = Some rec_expr; deps = dependency_list } in
        constructed_nodes := recursive_node :: !constructed_nodes
    
    | SSignal (signal_name, signal_type) ->
        let new_id = add_node (NSignal (signal_name, signal_type)) None [] in
        Hashtbl.add name_lookup_table signal_name new_id;
        source_node_ids := new_id :: !source_node_ids
    
    | SCell (cell_name, cell_type, init_opt) ->
        let dependency_list = match init_opt with
          | Some initializer_expr -> resolve_deps (free_vars initializer_expr)
          | None -> []
        in
        let new_id = add_node (NCell (cell_name, cell_type)) init_opt dependency_list in
        Hashtbl.add name_lookup_table cell_name new_id
    
    | SStream (name_opt, stream_def) ->
        let stream_name = match name_opt with
          | Some provided_name -> provided_name
          | None -> Printf.sprintf "_stream_%d" (get_id ())
        in
        let dependency_list = resolve_deps (stream_free_vars stream_def) in
        let new_id = add_node (NStream (stream_name, stream_def)) (Some (EStream stream_def)) dependency_list in
        Hashtbl.add name_lookup_table stream_name new_id;
        source_node_ids := new_id :: !source_node_ids
    
    | STypeDef _ | STimeAlias _ | STick _ | SImport _ ->
        ()  (* these don't create nodes *)
    
    | SExpr bare_expr ->
        let dependency_list = resolve_deps (free_vars bare_expr) in
        let _ = add_node (NExpr bare_expr) (Some bare_expr) dependency_list in
        ()
  in
  
  (* process all statements *)
  List.iter process_stmt program_stmts;
  
  (* convert to array (reverse to maintain declaration order) *)
  let nodes_as_array = Array.of_list (List.rev !constructed_nodes) in
  
  {
    nodes = nodes_as_array;
    node_count = Array.length nodes_as_array;
    name_to_id = name_lookup_table;
    sources = List.rev !source_node_ids;
  }


(* helper functions for querying the graph *)

let get_node graph_to_query node_id =
  if node_id >= 0 && node_id < graph_to_query.node_count then
    Some graph_to_query.nodes.(node_id)
  else
    None

let find_node_by_name graph_to_search target_name =
  match Hashtbl.find_opt graph_to_search.name_to_id target_name with
  | Some found_id -> get_node graph_to_search found_id
  | None -> None

let node_name node_to_name =
  match node_to_name.kind with
  | NSignal (name, _) | NLet (name, _) | NCell (name, _) | NStream (name, _) -> name
  | NExpr _ -> Printf.sprintf "_expr_%d" node_to_name.id

let is_source node_to_check =
  match node_to_check.kind with
  | NSignal _ | NStream _ -> true
  | _ -> false


(* pretty printing for debugging *)

let print_graph graph_to_print =
  Printf.printf "dependency graph:\n";
  Printf.printf "nodes: %d\n" graph_to_print.node_count;
  Printf.printf "sources: %d\n\n" (List.length graph_to_print.sources);
  
  Printf.printf "%-5s %-20s %-15s %s\n" "ID" "Name" "Kind" "Dependencies";
  Printf.printf "%s\n" (String.make 70 '-');
  
  Array.iter (fun current_node ->
    let kind_string = match current_node.kind with
      | NSignal _ -> "signal"
      | NStream _ -> "stream"
      | NLet _ -> "let"
      | NCell _ -> "cell"
      | NExpr _ -> "expr"
    in
    let deps_string = String.concat ", " 
      (List.map string_of_int current_node.deps)
    in
    Printf.printf "%-5d %-20s %-15s [%s]\n"
      current_node.id (node_name current_node) kind_string deps_string
  ) graph_to_print.nodes;
  
  Printf.printf "\nSources: [%s]\n"
    (String.concat ", " (List.map string_of_int graph_to_print.sources))
