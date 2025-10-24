(* pretty.ml - prints AST in readable format for debugging *)

open Ast
open Printf

(* indentation helper *)
let indent level = String.make (level * 2) ' '


(* type pretty printing *)
let rec pp_ty = function
  | TyBase TInt -> "int"
  | TyBase TFloat -> "float"
  | TyBase TBool -> "bool"
  | TyBase TString -> "string"
  | TyBase TChar -> "char"
  | TyBase TUnit -> "unit"
  | TySignal inner_ty -> sprintf "signal %s" (pp_ty inner_ty)
  | TyTuple type_elements -> sprintf "(%s)" (String.concat " * " (List.map pp_ty type_elements))
  | TyArrow (arg_ty, ret_ty) -> sprintf "%s -> %s" (pp_ty_atom arg_ty) (pp_ty ret_ty)
  | TyList elem_ty -> sprintf "%s list" (pp_ty_atom elem_ty)
  | TyRecord field_list ->
      sprintf "{ %s }" (String.concat "; " 
        (List.map (fun (name, ty) -> name ^ ": " ^ pp_ty ty) field_list))
  | TyVariant type_name -> type_name
  | TyVar var_name -> "'" ^ var_name

and pp_ty_atom = function
  | TyArrow _ as arrow_ty -> sprintf "(%s)" (pp_ty arrow_ty)
  | other_ty -> pp_ty other_ty


(* literals *)
let pp_literal = function
  | LInt n -> string_of_int n
  | LFloat f -> string_of_float f
  | LBool b -> string_of_bool b
  | LString s -> sprintf "\"%s\"" (String.escaped s)
  | LChar c -> sprintf "'%s'" (Char.escaped c)
  | LUnit -> "()"
  | LTimeMs n -> sprintf "%dms" n
  | LRateHz n -> sprintf "%dHz" n
  | LTicks n -> sprintf "%dticks" n

(* operators *)
let pp_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | FAdd -> "+."
  | FSub -> "-."
  | FMul -> "*."
  | FDiv -> "/."
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="
  | And -> "&&"
  | Or -> "||"
  | Pipe -> "|>"
  | Compose -> ">>"

let pp_unop = function
  | Neg -> "-"
  | FNeg -> "-."
  | Not -> "not"

let pp_temporal_op = function
  | Prev -> "prev"
  | Window n -> sprintf "window(%d)" n
  | Sample -> "sample"


(* patterns *)
let rec pp_pattern = function
  | PVar name -> name
  | PWild -> "_"
  | PLit lit -> pp_literal lit
  | PTuple patterns -> sprintf "(%s)" (String.concat ", " (List.map pp_pattern patterns))
  | PRecord field_patterns ->
      sprintf "{ %s }" (String.concat "; "
        (List.map (fun (name, pat) -> name ^ " = " ^ pp_pattern pat) field_patterns))
  | PVariant (ctor, None) -> ctor
  | PVariant (ctor, Some arg_pat) -> sprintf "%s %s" ctor (pp_pattern_atom arg_pat)
  | PAs (pat, alias) -> sprintf "%s as %s" (pp_pattern pat) alias
  | POr (p1, p2) -> sprintf "%s | %s" (pp_pattern p1) (pp_pattern p2)

and pp_pattern_atom = function
  | PVar _ | PWild | PLit _ as simple_pat -> pp_pattern simple_pat
  | complex_pat -> sprintf "(%s)" (pp_pattern complex_pat)


(* expressions *)
let rec pp_expr level e =
  let paren str = if level > 0 then sprintf "(%s)" str else str in
  match e with
  | ELit lit -> pp_literal lit
  | EVar name -> name
  | EUnary (op, operand) -> paren (sprintf "%s %s" (pp_unop op) (pp_expr 1 operand))
  | EBinary (op, left, right) ->
      paren (sprintf "%s %s %s" (pp_expr 1 left) (pp_binop op) (pp_expr 1 right))
  | ETemporal (op, operand) ->
      paren (sprintf "%s %s" (pp_temporal_op op) (pp_expr 1 operand))
  | EApply (func, arg) ->
      paren (sprintf "%s %s" (pp_expr 1 func) (pp_expr 2 arg))
  | ELambda (params, body) ->
      paren (sprintf "fun %s -> %s"
        (String.concat " " (List.map pp_pattern params))
        (pp_expr 0 body))
  | EIf (cond, then_branch, else_branch) ->
      paren (sprintf "if %s then %s else %s"
        (pp_expr 0 cond) (pp_expr 0 then_branch) (pp_expr 0 else_branch))
  | ELet (name, ty_opt, bound, body) ->
      let ty_annotation = match ty_opt with
        | None -> ""
        | Some t -> " : " ^ pp_ty t
      in
      paren (sprintf "let %s%s = %s in %s"
        name ty_annotation (pp_expr 0 bound) (pp_expr 0 body))
  | ELetRec (name, ty_opt, bound, body) ->
      let ty_annotation = match ty_opt with
        | None -> ""
        | Some t -> " : " ^ pp_ty t
      in
      paren (sprintf "let rec %s%s = %s in %s"
        name ty_annotation (pp_expr 0 bound) (pp_expr 0 body))
  | ETuple elements ->
      sprintf "(%s)" (String.concat ", " (List.map (pp_expr 0) elements))
  | ERecord fields ->
      sprintf "{ %s }" (String.concat "; "
        (List.map (fun (name, expr) -> name ^ " = " ^ pp_expr 0 expr) fields))
  | EField (record, field) ->
      sprintf "%s.%s" (pp_expr 2 record) field
  | EList [] -> "[]"
  | EList elements ->
      sprintf "[%s]" (String.concat "; " (List.map (pp_expr 0) elements))
  | EListCons (head, tail) ->
      paren (sprintf "%s :: %s" (pp_expr 1 head) (pp_expr 0 tail))
  | EMatch (scrutinee, cases) ->
      let pp_case (pat, guard, case_body) =
        let guard_str = match guard with
          | None -> ""
          | Some g -> " when " ^ pp_expr 0 g
        in
        sprintf "| %s%s -> %s" (pp_pattern pat) guard_str (pp_expr 0 case_body)
      in
      sprintf "match %s with\n%s"
        (pp_expr 0 scrutinee)
        (String.concat "\n" (List.map pp_case cases))
  | EBlock (stmts, final_expr) ->
      sprintf "{\n%s\n%s\n}"
        (String.concat "\n" (List.map (pp_stmt 1) stmts))
        (pp_expr 0 final_expr)
  | EStream sd -> pp_stream_def sd
  | ESampleEvery (expr, stream_name) ->
      paren (sprintf "sample %s every %s" (pp_expr 1 expr) stream_name)


(* stream definitions *)
and pp_stream_def stream_def =
  let pp_state (pat, init_expr) =
    sprintf "start %s = %s" (pp_pattern pat) (pp_expr 0 init_expr)
  in
  let pp_update (pat, update_expr) =
    sprintf "%s <- %s" (pp_pattern pat) (pp_expr 0 update_expr)
  in
  let states_str = String.concat ",\n  " (List.map pp_state stream_def.stream_state) in
  let updates_str = String.concat ",\n  " (List.map pp_update stream_def.stream_updates) in
  let emit_str = match stream_def.stream_emit with
    | None -> ""
    | Some emit_expr -> ",\n  emit " ^ pp_expr 0 emit_expr
  in
  let cadence_str = match stream_def.stream_cadence with
    | None -> ""
    | Some name -> " every " ^ name
  in
  sprintf "stream [\n  %s,\n  %s%s\n]%s"
    states_str updates_str emit_str cadence_str


(* statements *)
and pp_stmt indentation = function
  | SLet (name, ty_opt, expr) ->
      let ty_annotation = match ty_opt with
        | None -> ""
        | Some t -> " : " ^ pp_ty t
      in
      sprintf "%slet %s%s = %s;" (indent indentation) name ty_annotation (pp_expr 0 expr)
  | SLetRec (name, ty_opt, expr) ->
      let ty_annotation = match ty_opt with
        | None -> ""
        | Some t -> " : " ^ pp_ty t
      in
      sprintf "%slet rec %s%s = %s;" (indent indentation) name ty_annotation (pp_expr 0 expr)
  | SSignal (name, ty) ->
      sprintf "%ssignal %s : %s;" (indent indentation) name (pp_ty ty)
  | SCell (name, ty, init_opt) ->
      let init_str = match init_opt with
        | None -> ""
        | Some init_expr -> " := " ^ pp_expr 0 init_expr
      in
      sprintf "%scell %s : %s%s;" (indent indentation) name (pp_ty ty) init_str
  | STypeDef (name, typedef) ->
      sprintf "%stype %s = %s;" (indent indentation) name (pp_type_def typedef)
  | STimeAlias (name, lit) ->
      sprintf "%stime %s = %s;" (indent indentation) name (pp_literal lit)
  | STick expr ->
      sprintf "%stick every %s;" (indent indentation) (pp_expr 0 expr)
  | SStream (name_opt, stream_def) ->
      let name_part = match name_opt with
        | None -> ""
        | Some n -> n ^ " = "
      in
      sprintf "%s%s%s;" (indent indentation) name_part (pp_stream_def stream_def)
  | SImport (path, alias_opt) ->
      let alias_str = match alias_opt with
        | None -> ""
        | Some a -> " as " ^ a
      in
      sprintf "%simport %s%s;" (indent indentation) (String.concat "." path) alias_str
  | SExpr expr ->
      sprintf "%s%s;" (indent indentation) (pp_expr 0 expr)

and pp_type_def = function
  | TDVariant variant_cases ->
      String.concat " | " (List.map (fun (ctor, ty_opt) ->
        match ty_opt with
        | None -> ctor
        | Some t -> ctor ^ " of " ^ pp_ty t
      ) variant_cases)
  | TDRecord field_defs ->
      sprintf "{ %s }" (String.concat "; "
        (List.map (fun (name, ty) -> name ^ ": " ^ pp_ty ty) field_defs))
  | TDAlias ty -> pp_ty ty


(* program *)
let pp_program program =
  String.concat "\n\n" (List.map (pp_stmt 0) program)

let print_program program =
  print_endline "parsed AST:";
  print_endline (pp_program program);
  print_endline ""
