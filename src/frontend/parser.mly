%{
(* parser.mly - grammar for rolang *)

open Ast

%}

/* token declarations */

%token <int> INT_LIT TIME_MS TIME_HZ TIME_TICKS
%token <float> FLOAT_LIT
%token <string> STRING_LIT IDENT
%token <char> CHAR_LIT
%token <bool> BOOL_LIT

/* keywords */
%token IF THEN ELSE MATCH WITH WHEN
%token LET REC IN
%token SIGNAL CELL TYPE IMPORT AS OF
%token STREAM START EMIT EVERY
%token TIME TICK
%token PREV WINDOW SAMPLE
%token AND OR NOT
%token FUN

/* type keywords */
%token TINT TFLOAT TBOOL TSTRING TCHAR TUNIT TLIST

/* operators */
%token PLUS MINUS STAR SLASH PERCENT
%token FPLUS FMINUS FSTAR FSLASH
%token EQ EQEQ NEQ LT GT LE GE
%token ARROW LARROW PIPE COMPOSE CONS

/* delimiters */
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token COMMA SEMI COLON COLONEQ DOT UNDERSCORE BAR

%token EOF

/* precedence rules */
%right ARROW
%left PIPE
%left COMPOSE
%left OR
%left AND
%left EQEQ NEQ
%left LT GT LE GE
%right CONS
%left PLUS MINUS FPLUS FMINUS
%left STAR SLASH PERCENT FSTAR FSLASH
%right NOT
%left DOT
%nonassoc PREV WINDOW SAMPLE

%start program
%type <Ast.program> program

%%

/* top level */

program:
  | stmt_list EOF { $1 }

stmt_list:
  | /* empty */ { [] }
  | stmt SEMI stmt_list { $1 :: $3 }


/* statements */

stmt:
  | LET IDENT type_annotation_opt EQ expr
    { SLet ($2, $3, $5) }
  
  | LET REC IDENT type_annotation_opt EQ expr
    { SLetRec ($3, $4, $6) }
  
  | SIGNAL IDENT COLON ty
    { SSignal ($2, $4) }
  
  | CELL IDENT COLON ty cell_init_opt
    { SCell ($2, $4, $5) }
  
  | TYPE IDENT EQ type_def
    { STypeDef ($2, $4) }
  
  | TIME IDENT EQ time_literal
    { STimeAlias ($2, $4) }
  
  | TICK EVERY expr
    { STick $3 }
  
  | IMPORT ident_path import_alias_opt
    { SImport ($2, $3) }

cell_init_opt:
  | /* empty */ { None }
  | COLONEQ expr { Some $2 }

import_alias_opt:
  | /* empty */ { None }
  | AS IDENT { Some $2 }

type_annotation_opt:
  | /* empty */ { None }
  | COLON ty { Some $2 }

ident_path:
  | IDENT { [$1] }
  | IDENT DOT ident_path { $1 :: $3 }


/* type definitions */

type_def:
  | variant_cases { TDVariant $1 }
  | LBRACE record_field_list RBRACE { TDRecord $2 }

variant_cases:
  | variant_case { [$1] }
  | variant_case BAR variant_cases { $1 :: $3 }

variant_case:
  | IDENT { ($1, None) }
  | IDENT OF ty { ($1, Some $3) }

record_field_list:
  | /* empty */ { [] }
  | record_field { [$1] }
  | record_field SEMI record_field_list { $1 :: $3 }

record_field:
  | IDENT COLON ty { ($1, $3) }


/* stream definitions */

stream_def:
  | STREAM LBRACK stream_elements RBRACK stream_cadence_opt
    { let rec separate elems starts updates emit_val =
        match elems with
        | [] -> (List.rev starts, List.rev updates, emit_val)
        | `Start (p, e) :: rest -> separate rest ((p, e) :: starts) updates emit_val
        | `Update (p, e) :: rest -> separate rest starts ((p, e) :: updates) emit_val
        | `Emit e :: rest -> separate rest starts updates (Some e)
      in
      let (starts, updates, emit_val) = separate $3 [] [] None in
      { stream_state = starts;
        stream_updates = updates;
        stream_emit = emit_val;
        stream_cadence = $5; } }

stream_elements:
  | stream_element { [$1] }
  | stream_element COMMA stream_elements { $1 :: $3 }

stream_element:
  | START simple_pattern EQ expr { `Start ($2, $4) }
  | simple_pattern LARROW expr { `Update ($1, $3) }
  | EMIT expr { `Emit $2 }

stream_cadence_opt:
  | /* empty */ { None }
  | EVERY IDENT { Some $2 }


/* expressions */

expr:
  | simple_expr { $1 }
  
  /* binary operators */
  | expr PLUS expr { EBinary (Add, $1, $3) }
  | expr MINUS expr { EBinary (Sub, $1, $3) }
  | expr STAR expr { EBinary (Mul, $1, $3) }
  | expr SLASH expr { EBinary (Div, $1, $3) }
  | expr PERCENT expr { EBinary (Mod, $1, $3) }
  
  | expr FPLUS expr { EBinary (FAdd, $1, $3) }
  | expr FMINUS expr { EBinary (FSub, $1, $3) }
  | expr FSTAR expr { EBinary (FMul, $1, $3) }
  | expr FSLASH expr { EBinary (FDiv, $1, $3) }
  
  | expr EQEQ expr { EBinary (Eq, $1, $3) }
  | expr NEQ expr { EBinary (Neq, $1, $3) }
  | expr LT expr { EBinary (Lt, $1, $3) }
  | expr GT expr { EBinary (Gt, $1, $3) }
  | expr LE expr { EBinary (Le, $1, $3) }
  | expr GE expr { EBinary (Ge, $1, $3) }
  
  | expr AND expr { EBinary (And, $1, $3) }
  | expr OR expr { EBinary (Or, $1, $3) }
  
  | expr PIPE expr { EBinary (Pipe, $1, $3) }
  | expr COMPOSE expr { EBinary (Compose, $1, $3) }
  
  | expr CONS expr { EListCons ($1, $3) }
  
  /* unary operators */
  | NOT expr { EUnary (Not, $2) }
  | MINUS expr { EUnary (Neg, $2) }
  | FMINUS expr { EUnary (FNeg, $2) }
  
  /* temporal operators */
  | PREV expr { ETemporal (Prev, $2) }
  | WINDOW expr INT_LIT { ETemporal (Window $3, $2) }
  | SAMPLE expr EVERY IDENT { ESampleEvery ($2, $4) }
  
  /* function application */
  | expr simple_expr { EApply ($1, $2) }
  
  /* field access */
  | expr DOT IDENT { EField ($1, $3) }
  
  /* if-then-else */
  | IF expr THEN expr ELSE expr { EIf ($2, $4, $6) }
  
  /* let expression */
  | LET IDENT type_annotation_opt EQ expr IN expr
    { ELet ($2, $3, $5, $7) }
  
  | LET REC IDENT type_annotation_opt EQ expr IN expr
    { ELetRec ($3, $4, $6, $8) }
  
  /* match */
  | MATCH expr WITH match_cases { EMatch ($2, $4) }

simple_expr:
  | literal { ELit $1 }
  | time_literal { ELit $1 }
  | IDENT { EVar $1 }
  | LPAREN RPAREN { ELit LUnit }
  | LPAREN expr RPAREN { $2 }
  
  /* lambda */
  | FUN lambda_patterns ARROW expr { ELambda ($2, $4) }
  
  /* tuple */
  | LPAREN expr COMMA expr_list RPAREN { ETuple ($2 :: $4) }
  
  /* list */
  | LBRACK expr_list_semi RBRACK { EList $2 }
  
  /* record */
  | LBRACE record_expr_field_list RBRACE { ERecord $2 }
  
  /* stream expression */
  | stream_def { EStream $1 }

expr_list:
  | expr { [$1] }
  | expr COMMA expr_list { $1 :: $3 }

expr_list_semi:
  | /* empty */ { [] }
  | expr { [$1] }
  | expr SEMI expr_list_semi { $1 :: $3 }

record_expr_field_list:
  | /* empty */ { [] }
  | record_expr_field { [$1] }
  | record_expr_field SEMI record_expr_field_list { $1 :: $3 }

record_expr_field:
  | IDENT EQ expr { ($1, $3) }

lambda_patterns:
  | simple_pattern { [$1] }
  | simple_pattern lambda_patterns { $1 :: $2 }


/* pattern matching */

match_cases:
  | match_case { [$1] }
  | BAR match_case { [$2] }
  | match_case BAR match_cases { $1 :: $3 }
  | BAR match_case BAR match_cases { $2 :: $4 }

match_case:
  | pattern ARROW expr { ($1, None, $3) }
  | pattern WHEN expr ARROW expr { ($1, Some $3, $5) }


/* patterns */

pattern:
  | simple_pattern { $1 }
  | pattern CONS pattern { PVariant ("::", Some (PTuple [$1; $3])) }
  | pattern AS IDENT { PAs ($1, $3) }
  | pattern BAR pattern { POr ($1, $3) }

simple_pattern:
  | IDENT { PVar $1 }
  | UNDERSCORE { PWild }
  | literal { PLit $1 }
  | LBRACK RBRACK { PVariant ("[]", None) }
  | LPAREN pattern COMMA pattern_list RPAREN { PTuple ($2 :: $4) }
  | IDENT simple_pattern { PVariant ($1, Some $2) }
  | LPAREN pattern RPAREN { $2 }

pattern_list:
  | pattern { [$1] }
  | pattern COMMA pattern_list { $1 :: $3 }


/* types */

ty:
  | ty_arrow { $1 }

ty_arrow:
  | ty_tuple ARROW ty_arrow { TyArrow ($1, $3) }
  | ty_tuple { $1 }

ty_tuple:
  | ty_atom STAR ty_tuple_rest { TyTuple ($1 :: $3) }
  | ty_atom { $1 }

ty_tuple_rest:
  | ty_atom { [$1] }
  | ty_atom STAR ty_tuple_rest { $1 :: $3 }

ty_atom:
  | TINT { TyBase TInt }
  | TFLOAT { TyBase TFloat }
  | TBOOL { TyBase TBool }
  | TSTRING { TyBase TString }
  | TCHAR { TyBase TChar }
  | TUNIT { TyBase TUnit }
  | SIGNAL ty_atom { TySignal $2 }
  | ty_atom TLIST { TyList $1 }
  | TLIST ty_atom { TyList $2 }
  | IDENT { TyVariant $1 }
  | LPAREN ty RPAREN { $2 }


/* literals */

literal:
  | INT_LIT { LInt $1 }
  | FLOAT_LIT { LFloat $1 }
  | BOOL_LIT { LBool $1 }
  | STRING_LIT { LString $1 }
  | CHAR_LIT { LChar $1 }

time_literal:
  | TIME_MS { LTimeMs $1 }
  | TIME_HZ { LRateHz $1 }
  | TIME_TICKS { LTicks $1 }

%%
