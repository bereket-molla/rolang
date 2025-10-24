(* test_driver.ml - frontend testing utility
   
   shows all the tokens from lexer and the parsed AST
*)

open Printf
open Lexing

(* convert tokens to strings for display *)
let string_of_token = function
  | Parser.INT_LIT n -> sprintf "INT_LIT(%d)" n
  | Parser.FLOAT_LIT f -> sprintf "FLOAT_LIT(%g)" f
  | Parser.STRING_LIT s -> sprintf "STRING_LIT(\"%s\")" s
  | Parser.CHAR_LIT c -> sprintf "CHAR_LIT('%c')" c
  | Parser.BOOL_LIT b -> sprintf "BOOL_LIT(%b)" b
  | Parser.TIME_MS n -> sprintf "TIME_MS(%d)" n
  | Parser.TIME_HZ n -> sprintf "TIME_HZ(%d)" n
  | Parser.TIME_TICKS n -> sprintf "TIME_TICKS(%d)" n
  | Parser.IDENT s -> sprintf "IDENT(%s)" s
  
  | Parser.IF -> "IF"
  | Parser.THEN -> "THEN"
  | Parser.ELSE -> "ELSE"
  | Parser.MATCH -> "MATCH"
  | Parser.WITH -> "WITH"
  | Parser.WHEN -> "WHEN"
  | Parser.LET -> "LET"
  | Parser.REC -> "REC"
  | Parser.IN -> "IN"
  | Parser.SIGNAL -> "SIGNAL"
  | Parser.CELL -> "CELL"
  | Parser.TYPE -> "TYPE"
  | Parser.IMPORT -> "IMPORT"
  | Parser.AS -> "AS"
  | Parser.OF -> "OF"
  | Parser.STREAM -> "STREAM"
  | Parser.START -> "START"
  | Parser.EMIT -> "EMIT"
  | Parser.EVERY -> "EVERY"
  | Parser.TIME -> "TIME"
  | Parser.TICK -> "TICK"
  | Parser.PREV -> "PREV"
  | Parser.WINDOW -> "WINDOW"
  | Parser.SAMPLE -> "SAMPLE"
  | Parser.AND -> "AND"
  | Parser.OR -> "OR"
  | Parser.NOT -> "NOT"
  | Parser.FUN -> "FUN"
  
  | Parser.TINT -> "TINT"
  | Parser.TFLOAT -> "TFLOAT"
  | Parser.TBOOL -> "TBOOL"
  | Parser.TSTRING -> "TSTRING"
  | Parser.TCHAR -> "TCHAR"
  | Parser.TUNIT -> "TUNIT"
  | Parser.TLIST -> "TLIST"
  
  | Parser.PLUS -> "PLUS"
  | Parser.MINUS -> "MINUS"
  | Parser.STAR -> "STAR"
  | Parser.SLASH -> "SLASH"
  | Parser.PERCENT -> "PERCENT"
  | Parser.FPLUS -> "FPLUS"
  | Parser.FMINUS -> "FMINUS"
  | Parser.FSTAR -> "FSTAR"
  | Parser.FSLASH -> "FSLASH"
  | Parser.EQ -> "EQ"
  | Parser.EQEQ -> "EQEQ"
  | Parser.NEQ -> "NEQ"
  | Parser.LT -> "LT"
  | Parser.GT -> "GT"
  | Parser.LE -> "LE"
  | Parser.GE -> "GE"
  | Parser.ARROW -> "ARROW"
  | Parser.LARROW -> "LARROW"
  | Parser.PIPE -> "PIPE"
  | Parser.COMPOSE -> "COMPOSE"
  | Parser.CONS -> "CONS"
  
  | Parser.LPAREN -> "LPAREN"
  | Parser.RPAREN -> "RPAREN"
  | Parser.LBRACE -> "LBRACE"
  | Parser.RBRACE -> "RBRACE"
  | Parser.LBRACK -> "LBRACK"
  | Parser.RBRACK -> "RBRACK"
  | Parser.COMMA -> "COMMA"
  | Parser.SEMI -> "SEMI"
  | Parser.COLON -> "COLON"
  | Parser.COLONEQ -> "COLONEQ"
  | Parser.DOT -> "DOT"
  | Parser.UNDERSCORE -> "UNDERSCORE"
  | Parser.BAR -> "BAR"
  
  | Parser.EOF -> "EOF"

(* format position for error messages *)
let string_of_position pos =
  sprintf "%s:%d:%d" 
    pos.pos_fname 
    pos.pos_lnum 
    (pos.pos_cnum - pos.pos_bol + 1)

(* test the lexer by printing all tokens *)
let test_lexer input_filename =
  let input_channel = open_in input_filename in
  let lexer_buffer = Lexing.from_channel input_channel in
  lexer_buffer.lex_curr_p <- { lexer_buffer.lex_curr_p with pos_fname = input_filename };
  
  printf "lexer output:\n";
  printf "%-20s  %-15s %s\n" "Position" "Token" "Lexeme";
  printf "%s\n" (String.make 60 '-');
  
  let rec process_tokens () =
    try
      let current_token = Scanner.token lexer_buffer in
      let pos_string = string_of_position lexer_buffer.lex_start_p in
      let lexeme_string = Lexing.lexeme lexer_buffer in
      printf "%-20s %-15s %s\n" pos_string (string_of_token current_token) lexeme_string;
      if current_token = Parser.EOF then ()
      else process_tokens ()
    with
    | Scanner.LexError error_msg ->
        eprintf "lexical error at %s: %s\n" 
          (string_of_position lexer_buffer.lex_curr_p) error_msg;
        exit 1
  in
  process_tokens ();
  close_in input_channel;
  printf "\n"

(* test parser and type checker *)
let test_parser input_filename =
  let input_channel = open_in input_filename in
  let lexer_buffer = Lexing.from_channel input_channel in
  lexer_buffer.lex_curr_p <- { lexer_buffer.lex_curr_p with pos_fname = input_filename };
  
  try
    let parsed_ast = Parser.program Scanner.token lexer_buffer in
    close_in input_channel;
    Pretty.print_program parsed_ast;
    
    (* run type checker *)
    printf "\ntype checking:\n";
    (match Typecheck.typecheck_program parsed_ast with
     | Ok _final_env ->
         printf "passed\n";
         
         (* build dependency graph *)
         printf "\n";
         let dependency_graph = Graph.build_graph parsed_ast in
         Graph.print_graph dependency_graph;
         
         (* compute topological order *)
         (try
           let evaluation_order = Topo.topological_sort dependency_graph in
           Topo.print_topo_order dependency_graph evaluation_order;
           
           (* analyze forward cones *)
           Forward_cone.analyze_cones dependency_graph;
           
           (* export graph visualization *)
           printf "\n";
           Forward_cone.to_dot dependency_graph "rolang_graph.dot"
         with
         | Topo.Cycle cycle_msg ->
             printf "\n ycle detection failed:\n%s\n" cycle_msg;
             exit 1)
     | Error type_error_msg ->
         printf "type checking failed:\n%s\n" type_error_msg;
         exit 1)
  with
  | Scanner.LexError error_msg ->
      close_in input_channel;
      eprintf "lexical error at %s: %s\n" 
        (string_of_position lexer_buffer.lex_curr_p) error_msg;
      exit 1
  | Parsing.Parse_error ->
      close_in input_channel;
      eprintf "parse error at %s: unexpected token '%s'\n"
        (string_of_position lexer_buffer.lex_curr_p)
        (Lexing.lexeme lexer_buffer);
      exit 1

(* main *)
let () =
  if Array.length Sys.argv < 2 then begin
    eprintf "usage: %s <file.rol>\n" Sys.argv.(0);
    exit 1
  end;
  
  let input_filename = Sys.argv.(1) in
  let lex_only_mode = Array.length Sys.argv > 2 && Sys.argv.(2) = "--lex-only" in
  
  if not (Sys.file_exists input_filename) then begin
    eprintf "error: file '%s' not found\n" input_filename;
    exit 1
  end;
  
  printf "testing Rolang frontend on: %s\n\n" input_filename;
  
  (* test lexer *)
  test_lexer input_filename;
  
  (* test parser unless lex-only *)
  if not lex_only_mode then
    test_parser input_filename
