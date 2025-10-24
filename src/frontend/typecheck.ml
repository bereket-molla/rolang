(* typecheck.ml - type inference and checking for rolang
   
   handles signal type promotion, stream type inference,
   and all the pattern matching stuff
*)

open Ast

(* type environment management *)

module Env = Map.Make(String)

type env = {
  values: ty Env.t;  (* maps variable names to their types *)
  types: type_def Env.t;  (* user-defined types *)
  constructors: (string * ty option) Env.t;  (* variant constructors *)
}

let empty_env = {
  values = Env.empty;
  types = Env.empty;
  constructors = Env.empty;
}


(* expand type aliases - important for record types *)
let expand_type type_env ty_to_expand =
  match ty_to_expand with
  | TyVariant type_name ->
      (match Env.find_opt type_name type_env.types with
       | Some (TDRecord record_fields) -> TyRecord record_fields
       | Some (TDAlias aliased_type) -> aliased_type
       | Some (TDVariant _) -> ty_to_expand  (* keep it *)
       | None -> ty_to_expand)
  | other_type -> other_type

(* check if two types are equal, handling signal wrapping *)
let rec type_eq_internal type_env ty1 ty2 =
  let expanded_ty1 = expand_type type_env ty1 in
  let expanded_ty2 = expand_type type_env ty2 in
  match expanded_ty1, expanded_ty2 with
  | TyVar _, _ | _, TyVar _ -> true  (* type variables match anything for now *)
  | TyBase base1, TyBase base2 -> base1 = base2
  | TySignal sig1, TySignal sig2 -> type_eq_internal type_env sig1 sig2
  | TySignal sig_inner, other_ty | other_ty, TySignal sig_inner -> 
      type_eq_internal type_env sig_inner other_ty  (* signal promotion *)

  | TyTuple types1, TyTuple types2 ->
      List.length types1 = List.length types2 &&
      List.for_all2 (type_eq_internal type_env) types1 types2

  | TyArrow (arg1, ret1), TyArrow (arg2, ret2) ->
      type_eq_internal type_env arg1 arg2 && type_eq_internal type_env ret1 ret2

  | TyList elem_ty1, TyList elem_ty2 -> type_eq_internal type_env elem_ty1 elem_ty2
  
  | TyRecord fields1, TyRecord fields2 ->
      List.length fields1 = List.length fields2 &&
      List.for_all2 (fun (name1, ty1) (name2, ty2) -> 
        name1 = name2 && type_eq_internal type_env ty1 ty2) fields1 fields2

  | TyVariant name1, TyVariant name2 -> name1 = name2
  | _ -> false

(* wrapper for backwards compatibility *)
let type_eq ty1 ty2 = type_eq_internal empty_env ty1 ty2

(* promote to signal type if either operand is a signal *)
let promote_signal ty1 ty2 base_type =
  match ty1, ty2 with
  | TySignal _, _ | _, TySignal _ -> TySignal (TyBase base_type)
  | _ -> TyBase base_type

(* built-in functions available in all programs *)
let builtins = [
  ("print", TyArrow (TyBase TString, TyBase TUnit));
  ("println", TyArrow (TyBase TString, TyBase TUnit));
  ("int_of_float", TyArrow (TyBase TFloat, TyBase TInt));
  ("float_of_int", TyArrow (TyBase TInt, TyBase TFloat));
  ("string_of_int", TyArrow (TyBase TInt, TyBase TString));
  ("string_of_float", TyArrow (TyBase TFloat, TyBase TString));
  (* math functions *)
  ("min", TyArrow (TyBase TFloat, TyArrow (TyBase TFloat, TyBase TFloat)));
  ("max", TyArrow (TyBase TFloat, TyArrow (TyBase TFloat, TyBase TFloat)));
  ("abs", TyArrow (TyBase TFloat, TyBase TFloat));
  ("sqrt", TyArrow (TyBase TFloat, TyBase TFloat));
  ("sin", TyArrow (TyBase TFloat, TyBase TFloat));
  ("cos", TyArrow (TyBase TFloat, TyBase TFloat));
  (* list operations *)
  ("sum", TyArrow (TyList (TyBase TInt), TyBase TInt));
  ("all", TyArrow (TyList (TyBase TBool), TyBase TBool));
  ("any", TyArrow (TyList (TyBase TBool), TyBase TBool));
]

let init_env =
  List.fold_left
    (fun current_env (func_name, func_type) -> 
      { current_env with values = Env.add func_name func_type current_env.values })
    empty_env
    builtins


(* error handling *)

exception Type_error of string

let type_error error_message = raise (Type_error error_message)

let string_of_ty = Ast.string_of_ty

let type_mismatch expected_type got_type context_description =
  type_error (Printf.sprintf
    "Type mismatch in %s:\n  Expected: %s\n  Got:      %s"
    context_description (string_of_ty expected_type) (string_of_ty got_type))


(* pattern type checking - makes sure patterns match the scrutinee type *)

let rec check_pattern type_env pattern_to_check expected_type =
  match pattern_to_check with
  | PVar variable_name ->
      Env.add variable_name expected_type type_env.values
  
  | PWild ->
      type_env.values
  
  | PLit pattern_literal ->
      let literal_type = infer_literal pattern_literal in
      if not (type_eq_internal type_env literal_type expected_type) then
        type_mismatch expected_type literal_type "pattern literal";
      type_env.values
  
  | PTuple pattern_elements ->
      (match expected_type with
       | TyTuple type_elements when List.length pattern_elements = List.length type_elements ->
           List.fold_left2
             (fun current_values pat ty -> 
               check_pattern { type_env with values = current_values } pat ty)
             type_env.values pattern_elements type_elements
       | _ -> type_error "Tuple pattern type mismatch")
  
  | PRecord pattern_fields ->
      (match expected_type with
       | TyRecord expected_fields ->
           List.fold_left
             (fun current_values (field_name, field_pattern) ->
               let field_type = List.assoc field_name expected_fields in
               check_pattern { type_env with values = current_values } field_pattern field_type)
             type_env.values pattern_fields
       | _ -> type_error "Record pattern needs record type")
  
  | PVariant (constructor_name, arg_pattern_opt) ->
      (match Env.find_opt constructor_name type_env.constructors with
       | Some (variant_type_name, constructor_arg_type_opt) ->
           if not (type_eq_internal type_env expected_type (TyVariant variant_type_name)) then
             type_mismatch expected_type (TyVariant variant_type_name) "variant pattern";
           (match arg_pattern_opt, constructor_arg_type_opt with
            | None, None -> type_env.values
            | Some arg_pat, Some arg_ty ->
                check_pattern type_env arg_pat arg_ty
            | _ -> type_error (Printf.sprintf "Constructor %s arity mismatch" constructor_name))
       | None -> type_error (Printf.sprintf "Unknown constructor: %s" constructor_name))
  
  | PAs (inner_pattern, alias_name) ->
      let new_values = check_pattern type_env inner_pattern expected_type in
      Env.add alias_name expected_type new_values
  
  | POr (pattern1, pattern2) ->
      let values1 = check_pattern type_env pattern1 expected_type in
      let _values2 = check_pattern type_env pattern2 expected_type in
      (* both branches should bind same vars with same types *)
      values1


(* infer type of literals *)
and infer_literal lit =
  match lit with
  | LInt _ -> TyBase TInt
  | LFloat _ -> TyBase TFloat
  | LBool _ -> TyBase TBool
  | LString _ -> TyBase TString
  | LChar _ -> TyBase TChar
  | LUnit -> TyBase TUnit
  | LTimeMs _ | LRateHz _ | LTicks _ -> TyBase TInt

(* main type inference for expressions *)
let rec infer_expr type_env expression =
  match expression with
  | ELit lit_value -> infer_literal lit_value
  
  | EVar var_name ->
      (match Env.find_opt var_name type_env.values with
       | Some variable_type -> variable_type
       | None -> type_error (Printf.sprintf "Unbound variable: %s" var_name))
  
  | EUnary (operator, operand_expr) ->
      let operand_type = infer_expr type_env operand_expr in
      (match operator with
       | Neg | FNeg ->
           if type_eq operand_type (TyBase TInt) || type_eq operand_type (TyBase TFloat) then operand_type
           else type_error "Numeric negation requires int or float"
       | Not ->
           if type_eq operand_type (TyBase TBool) then TyBase TBool
           else type_mismatch (TyBase TBool) operand_type "logical not")
  
  | EBinary (bin_op, left_expr, right_expr) ->
      infer_binary type_env bin_op left_expr right_expr
  
  | ETemporal (temporal_operator, operand_expr) ->
      let operand_type = infer_expr type_env operand_expr in
      (match temporal_operator with
       | Prev -> operand_type  (* prev x has same type as x *)
       | Window _ -> TyList operand_type  (* window produces list *)
       | Sample -> operand_type)
  
  | EApply (function_expr, argument_expr) ->
      let function_type = infer_expr type_env function_expr in
      let argument_type = infer_expr type_env argument_expr in
      (match function_type with
       | TyArrow (parameter_type, result_type) ->
           if type_eq parameter_type argument_type then result_type
           else type_mismatch parameter_type argument_type "function application"
       | _ -> type_error (Printf.sprintf
           "Cannot apply non-function (type: %s)" (string_of_ty function_type)))
  
  | ELambda (parameters, body_expr) ->
      (* for simple patterns just use type variable for now *)
      let parameter_types = List.map (fun _ -> TyVar "a") parameters in
      let parameter_values = List.fold_left2
        (fun current_vals pat ty ->
          check_pattern { type_env with values = current_vals } pat ty)
        type_env.values parameters parameter_types
      in
      let body_type = infer_expr { type_env with values = parameter_values } body_expr in
      (* build arrow type *)
      (match parameters with
       | [_] -> TyArrow (TyVar "a", body_type)
       | _ ->
           (* multi-param gets curried *)
           List.fold_right
             (fun _ accumulated_type -> TyArrow (TyVar "a", accumulated_type))
             parameters body_type)
  
  | EIf (condition_expr, then_expr, else_expr) ->
      let condition_type = infer_expr type_env condition_expr in
      if not (type_eq condition_type (TyBase TBool)) then
        type_mismatch (TyBase TBool) condition_type "if condition";
      let then_type = infer_expr type_env then_expr in
      let else_type = infer_expr type_env else_expr in
      if not (type_eq then_type else_type) then
        type_mismatch then_type else_type "if branches";
      then_type
  
  | ELet (binding_name, type_annotation_opt, bound_expr, body_expr) ->
      let bound_expr_type = infer_expr type_env bound_expr in
      (match type_annotation_opt with
       | Some annotated_type ->
           if not (type_eq_internal type_env annotated_type bound_expr_type) then
             type_mismatch annotated_type bound_expr_type (Printf.sprintf "let %s" binding_name)
       | None -> ());
      let actual_binding_type = match type_annotation_opt with 
        | Some t -> t 
        | None -> bound_expr_type 
      in
      let extended_env = { type_env with values = Env.add binding_name actual_binding_type type_env.values } in
      infer_expr extended_env body_expr
  
  | ELetRec (function_name, type_annotation_opt, function_expr, body_expr) ->
      (* add name to env before checking for recursion *)
      let assumed_function_type = match type_annotation_opt with
        | Some annotated_ty -> annotated_ty
        | None -> TyArrow (TyBase TInt, TyBase TInt)  (* default assumption *)
      in
      let env_with_function = { type_env with values = Env.add function_name assumed_function_type type_env.values } in
      let inferred_function_type = infer_expr env_with_function function_expr in
      (match type_annotation_opt with
       | Some annotated_type ->
           if not (type_eq annotated_type inferred_function_type) then
             type_mismatch annotated_type inferred_function_type (Printf.sprintf "let rec %s" function_name)
       | None -> ());
      infer_expr env_with_function body_expr
  
  | ETuple tuple_elements ->
      TyTuple (List.map (infer_expr type_env) tuple_elements)
  
  | ERecord record_fields ->
      TyRecord (List.map (fun (field_name, field_expr) -> (field_name, infer_expr type_env field_expr)) record_fields)
  
  | EField (record_expr, field_name) ->
      let record_type = infer_expr type_env record_expr in
      let expanded_record_type = expand_type type_env record_type in
      (match expanded_record_type with
       | TyRecord field_types ->
           (match List.assoc_opt field_name field_types with
            | Some field_type -> field_type
            | None -> type_error (Printf.sprintf "No field %s in record" field_name))
       | _ -> type_error (Printf.sprintf
           "Field access requires record type, got %s (expanded from %s)"
           (string_of_ty expanded_record_type) (string_of_ty record_type)))
  
  | EList [] ->
      TyList (TyVar "a")  (* polymorphic empty list *)
  
  | EList (first_elem :: rest_elems) ->
      let first_type = infer_expr type_env first_elem in
      List.iter (fun elem ->
        let elem_type = infer_expr type_env elem in
        if not (type_eq first_type elem_type) then
          type_mismatch first_type elem_type "list element") rest_elems;
      TyList first_type
  
  | EListCons (head_expr, tail_expr) ->
      let head_type = infer_expr type_env head_expr in
      let tail_type = infer_expr type_env tail_expr in
      (match tail_type with
       | TyList element_type ->
           if type_eq head_type element_type then TyList element_type
           else type_mismatch element_type head_type "list cons"
       | _ -> type_error ":: requires list as second argument")
  
  | EMatch (scrutinee_expr, match_cases) ->
      let scrutinee_type = infer_expr type_env scrutinee_expr in
      let result_type_ref = ref None in
      List.iter (fun (case_pattern, guard_expr_opt, case_body) ->
        let pattern_bindings = check_pattern type_env case_pattern scrutinee_type in
        (match guard_expr_opt with
         | Some guard_expression ->
             let guard_type = infer_expr { type_env with values = pattern_bindings } guard_expression in
             if not (type_eq guard_type (TyBase TBool)) then
               type_mismatch (TyBase TBool) guard_type "match guard"
         | None -> ());
        let case_body_type = infer_expr { type_env with values = pattern_bindings } case_body in
        match !result_type_ref with
        | None -> result_type_ref := Some case_body_type
        | Some expected_result_type ->
            if not (type_eq expected_result_type case_body_type) then
              type_mismatch expected_result_type case_body_type "match case"
      ) match_cases;
      (match !result_type_ref with
       | Some result_ty -> result_ty
       | None -> type_error "Match with no cases")
  
  | EBlock (block_statements, final_expr) ->
      let env_after_stmts = check_stmts type_env block_statements in
      infer_expr env_after_stmts final_expr
  
  | EStream stream_definition ->
      infer_stream type_env stream_definition
  
  | ESampleEvery (sampled_expr, _stream_name) ->
      (* sample e every NAME has same type as e *)
      infer_expr type_env sampled_expr

and infer_binary type_env binary_operator left_expr right_expr =
  let left_type = infer_expr type_env left_expr in
  let right_type = infer_expr type_env right_expr in
  match binary_operator with
  | Add | Sub | Mul | Div | Mod ->
      if type_eq left_type (TyBase TInt) && type_eq right_type (TyBase TInt) then
        promote_signal left_type right_type TInt
      else type_error (Printf.sprintf
        "Integer arithmetic requires int operands, got %s and %s"
        (string_of_ty left_type) (string_of_ty right_type))
  
  | FAdd | FSub | FMul | FDiv ->
      if type_eq left_type (TyBase TFloat) && type_eq right_type (TyBase TFloat) then
        promote_signal left_type right_type TFloat
      else type_error "Float arithmetic requires float operands"
  
  | Eq | Neq ->
      if type_eq left_type right_type then TyBase TBool
      else type_mismatch left_type right_type "equality comparison"
  
  | Lt | Gt | Le | Ge ->
      if (type_eq left_type (TyBase TInt) && type_eq right_type (TyBase TInt)) ||
         (type_eq left_type (TyBase TFloat) && type_eq right_type (TyBase TFloat)) then
        TyBase TBool
      else type_error "Comparison requires numeric operands of same type"
  
  | And | Or ->
      if type_eq left_type (TyBase TBool) && type_eq right_type (TyBase TBool) then
        TyBase TBool
      else type_error "Logical operators require bool operands"
  
  | Pipe ->
      (* e1 |> e2 means e2 e1 *)
      (match right_type with
       | TyArrow (expected_param, pipe_result) ->
           if type_eq left_type expected_param then pipe_result
           else type_mismatch expected_param left_type "pipe"
       | _ -> type_error "Pipe requires function as second argument")
  
  | Compose ->
      (* e1 >> e2 means fun x -> e2 (e1 x) *)
      (match left_type, right_type with
       | TyArrow (a, b), TyArrow (b_prime, c) ->
           if type_eq b b_prime then TyArrow (a, c)
           else type_error "Function composition type mismatch"
       | _ -> type_error "Composition requires two functions")

and infer_stream type_env stream_def =
  (* check all start bindings, build initial env *)
  let state_values_env = List.fold_left
    (fun current_vals (start_pattern, init_expression) ->
      let init_value_type = infer_expr type_env init_expression in
      check_pattern { type_env with values = current_vals } start_pattern init_value_type)
    type_env.values
    stream_def.stream_state
  in
  
  (* check updates with prev available *)
  let env_with_state = { type_env with values = state_values_env } in
  List.iter (fun (update_pattern, update_expression) ->
    let update_expr_type = infer_expr env_with_state update_expression in
    (* pattern should match the state variable type *)
    let _pattern_env = check_pattern env_with_state update_pattern update_expr_type in
    ()
  ) stream_def.stream_updates;
  
  (* check emit expression *)
  match stream_def.stream_emit with
  | Some emit_expression ->
      let emitted_value_type = infer_expr env_with_state emit_expression in
      TySignal emitted_value_type  (* stream produces signal type *)
  | None ->
      TyBase TUnit  (* stream with no emit *)


(* statement type checking *)
and check_stmt type_env statement =
  match statement with
  | SLet (variable_name, type_annotation_opt, bound_expression) ->
      let expression_type = infer_expr type_env bound_expression in
      (match type_annotation_opt with
       | Some annotated_type ->
           if not (type_eq_internal type_env annotated_type expression_type) then
             type_mismatch annotated_type expression_type (Printf.sprintf "let %s" variable_name)
       | None -> ());
      let binding_type = match type_annotation_opt with Some t -> t | None -> expression_type in
      { type_env with values = Env.add variable_name binding_type type_env.values }
  
  | SLetRec (function_name, type_annotation_opt, function_expression) ->
      let assumed_type = match type_annotation_opt with
        | Some annotated_ty -> annotated_ty
        | None -> TyArrow (TyBase TInt, TyBase TInt)
      in
      let env_with_func = { type_env with values = Env.add function_name assumed_type type_env.values } in
      let inferred_expr_type = infer_expr env_with_func function_expression in
      (match type_annotation_opt with
       | Some annotated_type ->
           if not (type_eq annotated_type inferred_expr_type) then
             type_mismatch annotated_type inferred_expr_type (Printf.sprintf "let rec %s" function_name)
       | None -> ());
      env_with_func
  
  | SSignal (signal_name, signal_type) ->
      { type_env with values = Env.add signal_name (TySignal signal_type) type_env.values }
  
  | SCell (cell_name, cell_type, initializer_opt) ->
      (match initializer_opt with
       | Some init_expression ->
           let init_type = infer_expr type_env init_expression in
           if not (type_eq_internal type_env cell_type init_type) then
             type_mismatch cell_type init_type (Printf.sprintf "cell %s" cell_name)
       | None -> ());
      { type_env with values = Env.add cell_name cell_type type_env.values }
  
  | STypeDef (type_name, type_definition) ->
      let env_with_typedef = { type_env with types = Env.add type_name type_definition type_env.types } in
      (match type_definition with
       | TDVariant variant_constructors ->
           (* register constructors as both constructors and values *)
           List.fold_left
             (fun current_env (constructor_name, constructor_arg_type_opt) ->
               let constructor_type = match constructor_arg_type_opt with
                 | None -> TyVariant type_name  (* nullary constructor is the type itself *)
                 | Some arg_type -> TyArrow (arg_type, TyVariant type_name)  (* unary is a function *)
               in
               { current_env with 
                 constructors = Env.add constructor_name (type_name, constructor_arg_type_opt) current_env.constructors;
                 values = Env.add constructor_name constructor_type current_env.values })
             env_with_typedef variant_constructors
       | TDRecord _ | TDAlias _ -> env_with_typedef)
  
  | STimeAlias _ | STick _ | SImport _ ->
      type_env  (* these don't affect types *)
  
  | SStream (stream_name_opt, stream_definition) ->
      let stream_result_type = infer_stream type_env stream_definition in
      (match stream_name_opt with
       | Some stream_name -> { type_env with values = Env.add stream_name stream_result_type type_env.values }
       | None -> type_env)
  
  | SExpr expression ->
      let _ = infer_expr type_env expression in
      type_env

and check_stmts type_env statements =
  List.fold_left check_stmt type_env statements


(* public interface *)
let typecheck_program program_statements =
  try
    let final_environment = check_stmts init_env program_statements in
    Ok final_environment
  with
  | Type_error error_msg -> Error error_msg
  | other_exception -> Error (Printf.sprintf "Unexpected error: %s" (Printexc.to_string other_exception))
