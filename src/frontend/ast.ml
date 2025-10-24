(* ast.ml - abstract syntax tree for rolang
   
   represents the parsed program structure before we
   lower it to the intermediate representation
*)

(* base primitive types *)
type base_ty =
  | TInt
  | TFloat
  | TBool
  | TString
  | TChar
  | TUnit


(* type expressions - supports signals which are time-varying *)
type ty =
  | TyBase of base_ty
  | TySignal of ty  
  | TyTuple of ty list
  | TyArrow of ty * ty
  | TyList of ty
  | TyRecord of (string * ty) list
  | TyVariant of string  (* user defined types *)
  | TyVar of string  (* for polymorphism 'a *)


(* pattern matching constructs *)
type pattern =
  | PVar of string
  | PWild  (* underscore pattern *)
  | PLit of literal
  | PTuple of pattern list
  | PRecord of (string * pattern) list
  | PVariant of string * pattern option  (* constructor patterns *)
  | PAs of pattern * string
  | POr of pattern * pattern         


(* literal values in the source *)
and literal =
  | LInt of int
  | LFloat of float
  | LBool of bool
  | LString of string
  | LChar of char
  | LUnit
  | LTimeMs of int  (* time literals like 50ms *)
  | LRateHz of int  (* or 20Hz *)
  | LTicks of int      


(* binary operators *)
type binop =
  | Add | Sub | Mul | Div | Mod
  | FAdd | FSub | FMul | FDiv    (* float versions *)
  | Eq | Neq | Lt | Gt | Le | Ge
  | And | Or
  | Pipe   (* |> for pipelining *)
  | Compose  (* >> function composition *)

type unop =
  | Neg
  | FNeg
  | Not            


(* temporal operators - these are what make rolang reactive *)
type temporal_op =
  | Prev  (* get value from previous index *)
  | Window of int  (* ring buffer of last n values *)
  | Sample  (* sample at stream firing times *)


(* main expression type - this is the big one *)
type expr =
  | ELit of literal
  | EVar of string
  
  | EUnary of unop * expr
  | EBinary of binop * expr * expr
  
  (* temporal stuff *)
  | ETemporal of temporal_op * expr
  
  | EApply of expr * expr
  | ELambda of pattern list * expr
  
  | EIf of expr * expr * expr
  | ELet of string * ty option * expr * expr
  | ELetRec of string * ty option * expr * expr
  
  | ETuple of expr list
  | ERecord of (string * expr) list
  | EField of expr * string  (* field access *)
  | EList of expr list
  | EListCons of expr * expr  (* :: operator *)
  
  | EMatch of expr * (pattern * expr option * expr) list
                (* guards are optional in match cases *)
  
  | EBlock of stmt list * expr
  
  (* streams are first-class expressions *)
  | EStream of stream_def
  
  | ESampleEvery of expr * string    


(* stream block definition
   
   streams are periodic computations with local state that
   fire at a specific cadence. they're the main way to do
   time-based logic in rolang
   
   syntax: stream [ start x = 0, x <- x + 1, emit x ] every CTRL
*)
and stream_def = {
  stream_state : (pattern * expr) list;  (* local state variables and their init values *)
  stream_updates : (pattern * expr) list;  (* how to update state each firing *)
  stream_emit : expr option;  (* what to output *)
  stream_cadence : string option;  (* firing rate *)
}


(* top level statements *)
and stmt =
  | SLet of string * ty option * expr
  | SLetRec of string * ty option * expr
  | SSignal of string * ty  (* external input *)
  | SCell of string * ty * expr option  (* mutable cell *)
  
  | STypeDef of string * type_def
  
  | STimeAlias of string * literal  (* time CTRL = 50ms *)
  | STick of expr
  
  | SStream of string option * stream_def
  
  | SImport of string list * string option
  
  | SExpr of expr


(* type definitions - variants, records, or aliases *)
and type_def =
  | TDVariant of (string * ty option) list
  | TDRecord of (string * ty) list
  | TDAlias of ty                         


type program = stmt list


(* helper functions for working with types *)

let rec string_of_ty = function
  | TyBase TInt    -> "int"
  | TyBase TFloat  -> "float"
  | TyBase TBool   -> "bool"
  | TyBase TString -> "string"
  | TyBase TChar   -> "char"
  | TyBase TUnit   -> "unit"
  | TySignal type_param     -> "signal " ^ string_of_ty type_param
  | TyTuple type_list     -> "(" ^ String.concat " * " (List.map string_of_ty type_list) ^ ")"
  | TyArrow (param_type, return_type) -> string_of_ty param_type ^ " -> " ^ string_of_ty return_type
  | TyList element_type       -> string_of_ty element_type ^ " list"
  | TyRecord field_types -> 
      "{ " ^ String.concat "; " 
        (List.map (fun (field_name, field_type) -> field_name ^ ": " ^ string_of_ty field_type) field_types) ^ " }"
  | TyVariant type_name -> type_name
  | TyVar variable_name     -> "'" ^ variable_name

(* check if a type is a signal type *)
let is_signal_type = function
  | TySignal _ -> true
  | _ -> false

(* unwrap one layer of signal wrapper if present *)
let unwrap_signal = function
  | TySignal inner_type -> inner_type
  | other_type -> other_type


(* TODO: might want to add a way to wrap in signal only if not already wrapped *)
let ensure_signal = function
  | TySignal _ as already_signal -> already_signal
  | non_signal_type -> TySignal non_signal_type
