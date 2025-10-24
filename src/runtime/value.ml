(* value.ml - runtime value representation
   
   represents actual values during program execution
*)

open Ast

(* runtime value types *)
type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VString of string
  | VChar of char
  | VUnit
  | VTuple of value list
  | VRecord of (string * value) list
  | VList of value list
  | VVariant of string * value option  (* constructor name, optional arg *)
  | VClosure of pattern list * expr * value_env  (* params, body, closure *)

and value_env = (string * value) list


(* pretty printing for debugging *)
let rec string_of_value = function
  | VInt n -> string_of_int n
  | VFloat f -> string_of_float f
  | VBool b -> string_of_bool b
  | VString s -> Printf.sprintf "\"%s\"" s
  | VChar c -> Printf.sprintf "'%c'" c
  | VUnit -> "()"
  | VTuple values -> 
      Printf.sprintf "(%s)" (String.concat ", " (List.map string_of_value values))
  | VRecord field_values ->
      Printf.sprintf "{ %s }" (String.concat "; "
        (List.map (fun (field_name, field_value) -> field_name ^ " = " ^ string_of_value field_value) field_values))
  | VList value_list ->
      Printf.sprintf "[%s]" (String.concat "; " (List.map string_of_value value_list))
  | VVariant (constructor_name, None) -> constructor_name
  | VVariant (constructor_name, Some arg_value) -> 
      Printf.sprintf "%s(%s)" constructor_name (string_of_value arg_value)
  | VClosure _ -> "<function>"

(* type-safe extraction functions *)
let as_int = function
  | VInt n -> n
  | v -> failwith (Printf.sprintf "Expected int, got %s" (string_of_value v))

let as_float = function
  | VFloat f -> f
  | VInt n -> float_of_int n  (* auto-convert int to float *)
  | v -> failwith (Printf.sprintf "Expected float, got %s" (string_of_value v))

let as_bool = function
  | VBool b -> b
  | v -> failwith (Printf.sprintf "Expected bool, got %s" (string_of_value v))

let as_string = function
  | VString s -> s
  | v -> failwith (Printf.sprintf "Expected string, got %s" (string_of_value v))

let as_list = function
  | VList elements -> elements
  | v -> failwith (Printf.sprintf "Expected list, got %s" (string_of_value v))


(* equality policies for change detection *)

type eq_policy =
  | EqExact  (* exact equality for ints, bools, etc *)
  | EqEpsilon of float  (* float comparison with tolerance *)
  | EqStructural  (* recursive equality for records/lists *)

let default_epsilon = 1e-9

(* check equality using a policy *)
let rec value_eq equality_policy val1 val2 =
  match equality_policy, val1, val2 with
  (* exact *)
  | EqExact, VInt n1, VInt n2 -> n1 = n2
  | EqExact, VBool b1, VBool b2 -> b1 = b2
  | EqExact, VString s1, VString s2 -> s1 = s2
  | EqExact, VChar c1, VChar c2 -> c1 = c2
  | EqExact, VUnit, VUnit -> true
  | EqExact, VVariant (c1, None), VVariant (c2, None) -> c1 = c2
  
  (* epsilon for floats *)
  | EqEpsilon tolerance, VFloat f1, VFloat f2 -> abs_float (f1 -. f2) < tolerance
  
  (* structural for compound types *)
  | EqStructural, VTuple vals1, VTuple vals2 ->
      List.length vals1 = List.length vals2 &&
      List.for_all2 (value_eq EqStructural) vals1 vals2
  | EqStructural, VRecord fields1, VRecord fields2 ->
      List.length fields1 = List.length fields2 &&
      List.for_all2
        (fun (name1, val1) (name2, val2) -> name1 = name2 && value_eq EqStructural val1 val2)
        fields1 fields2
  | EqStructural, VList vals1, VList vals2 ->
      List.length vals1 = List.length vals2 &&
      List.for_all2 (value_eq EqStructural) vals1 vals2
  
  | _, _, _ -> false

(* pick default policy based on value type *)
let default_policy = function
  | VFloat _ -> EqEpsilon default_epsilon
  | VTuple _ | VRecord _ | VList _ -> EqStructural
  | _ -> EqExact

(* convenience wrapper *)
let values_equal v1 v2 =
  value_eq (default_policy v1) v1 v2


(* convert AST literals to runtime values *)
let value_of_literal = function
  | LInt n -> VInt n
  | LFloat f -> VFloat f
  | LBool b -> VBool b
  | LString s -> VString s
  | LChar c -> VChar c
  | LUnit -> VUnit
  | LTimeMs n | LRateHz n | LTicks n -> VInt n


(* create default values for types *)
let rec default_value type_to_default =
  match type_to_default with
  | TyBase TInt -> VInt 0
  | TyBase TFloat -> VFloat 0.0
  | TyBase TBool -> VBool false
  | TyBase TString -> VString ""
  | TyBase TChar -> VChar '\x00'
  | TyBase TUnit -> VUnit
  | TySignal inner_type -> default_value inner_type
  | TyTuple element_types -> VTuple (List.map default_value element_types)
  | TyList _ -> VList []
  | TyRecord field_defs -> 
      VRecord (List.map (fun (field_name, field_type) -> (field_name, default_value field_type)) field_defs)
  | TyVariant _ -> VUnit  (* will be overridden by actual constructor *)
  | TyArrow _ -> failwith "Cannot create default function value"
  | TyVar _ -> VUnit
