{
(* scanner.mll - lexer for rolang *)

open Parser
exception LexError of string

(* keywords mapped to tokens *)
let keyword_table = Hashtbl.create 64

let () =
  List.iter (fun (kw, tok) -> Hashtbl.add keyword_table kw tok)
    [
      ("if", IF);
      ("then", THEN);
      ("else", ELSE);
      ("match", MATCH);
      ("with", WITH);
      ("when", WHEN);
      ("let", LET);
      ("rec", REC);
      ("in", IN);
      ("signal", SIGNAL);
      ("cell", CELL);
      ("type", TYPE);
      ("import", IMPORT);
      ("as", AS);
      ("of", OF);
      ("stream", STREAM);
      ("start", START);
      ("emit", EMIT);
      ("every", EVERY);
      ("time", TIME);
      ("tick", TICK);
      (* temporal operators *)
      ("prev", PREV);
      ("window", WINDOW);
      ("sample", SAMPLE);
      ("and", AND);
      ("or", OR);
      ("not", NOT);
      (* type keywords *)
      ("int", TINT);
      ("float", TFLOAT);
      ("bool", TBOOL);
      ("string", TSTRING);
      ("char", TCHAR);
      ("unit", TUNIT);
      ("list", TLIST);
      ("fun", FUN);
      ("true", BOOL_LIT true);
      ("false", BOOL_LIT false);
    ]

(* unescape char sequences *)
let unescape_char = function
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'r' -> '\r'
  | 'b' -> '\b'
  | '\\' -> '\\'
  | '\'' -> '\''
  | '"' -> '"'
  | c -> c

let unescape_string s =
  let buf = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then Buffer.contents buf
    else if s.[i] = '\\' && i + 1 < String.length s then begin
      Buffer.add_char buf (unescape_char s.[i+1]);
      loop (i + 2)
    end else begin
      Buffer.add_char buf s.[i];
      loop (i + 1)
    end
  in
  loop 0
}

(* regexp macros *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident_start = alpha | '_'
let ident_char = alpha | digit | '_' | '\''
let whitespace = [' ' '\t' '\r']
let newline = '\n' | "\r\n"

let int_lit = digit+
let float_lit = digit+ '.' digit* (['e' 'E'] ['+' '-']? digit+)?

let time_ms = digit+ "ms"
let time_hz = digit+ ("Hz" | "hz")
let time_ticks = digit+ "ticks"

let escape_seq = '\\' ['n' 't' 'r' 'b' '\\' '\'' '"']
let string_char = [^ '"' '\\'] | escape_seq
let char_char = [^ '\'' '\\'] | escape_seq

(* main token rule *)
rule token = parse
  | whitespace+ { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  
  | "(*" { block_comment 1 lexbuf }
  
  | int_lit as i { INT_LIT (int_of_string i) }
  | float_lit as f { FLOAT_LIT (float_of_string f) }
  | time_ms as t { let num = int_of_string (String.sub t 0 (String.length t - 2)) in
                   TIME_MS num }
  | time_hz as t { let num = int_of_string (String.sub t 0 (String.length t - 2)) in
                   TIME_HZ num }
  | time_ticks as t { let num = int_of_string (String.sub t 0 (String.length t - 5)) in
                      TIME_TICKS num }
  
  | '"' { let buf = Buffer.create 32 in
          string_lit buf lexbuf }
  
  | '\'' (char_char as c) '\''
        { let unesc = if c.[0] = '\\' 
                      then unescape_char c.[1]
                      else c.[0] in
          CHAR_LIT unesc }
  
  | ident_start ident_char* as id
        { try Hashtbl.find keyword_table id
          with Not_found -> IDENT id }
  
  (* two-char operators *)
  | "->" { ARROW }
  | "|>" { PIPE }
  | ">>" { COMPOSE }
  | "<-" { LARROW }
  | ":=" { COLONEQ }
  | "==" { EQEQ }
  | "!=" { NEQ }
  | "<=" { LE }
  | ">=" { GE }
  | "::" { CONS }
  | "+." { FPLUS }
  | "-." { FMINUS }
  | "*." { FSTAR }
  | "/." { FSLASH }
  
  (* single-char operators *)
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { SLASH }
  | '%' { PERCENT }
  | '=' { EQ }
  | '<' { LT }
  | '>' { GT }
  | '|' { BAR }
  
  (* delimiters *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | ',' { COMMA }
  | ';' { SEMI }
  | ':' { COLON }
  | '.' { DOT }
  | '_' { UNDERSCORE }
  
  | eof { EOF }
  
  | _ as ch { raise (LexError (Printf.sprintf "Unexpected character: '%c'" ch)) }

(* handle nested comments *)
and block_comment depth = parse
  | "(*" { block_comment (depth + 1) lexbuf }
  | "*)" { if depth = 1 then token lexbuf 
           else block_comment (depth - 1) lexbuf }
  | newline { Lexing.new_line lexbuf; block_comment depth lexbuf }
  | eof { raise (LexError "Unterminated block comment") }
  | _ { block_comment depth lexbuf }

(* string literal parsing *)
and string_lit buf = parse
  | '"' { STRING_LIT (Buffer.contents buf) }
  | '\\' (_ as c) { Buffer.add_char buf (unescape_char c);
                    string_lit buf lexbuf }
  | newline { Lexing.new_line lexbuf;
              Buffer.add_char buf '\n';
              string_lit buf lexbuf }
  | eof { raise (LexError "Unterminated string") }
  | _ as c { Buffer.add_char buf c;
             string_lit buf lexbuf }
