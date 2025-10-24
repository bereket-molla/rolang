(* runtime_main.ml - simulated execution demo *)

open Lexing

let parse_file filename =
  let input_channel = open_in filename in
  let lexer_buffer = Lexing.from_channel input_channel in
  lexer_buffer.lex_curr_p <- { lexer_buffer.lex_curr_p with pos_fname = filename };
  
  try
    let parsed_program = Parser.program Scanner.token lexer_buffer in
    close_in input_channel;
    Ok parsed_program
  with
  | Scanner.LexError error_msg ->
      close_in input_channel;
      Error (Printf.sprintf "Lexical error: %s" error_msg)
  | Parsing.Parse_error ->
      close_in input_channel;
      Error (Printf.sprintf "Parse error at %s"
        (Lexing.lexeme lexer_buffer))

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <file.rol>\n" Sys.argv.(0);
    exit 1
  end;
  
  let input_filename = Sys.argv.(1) in
  Printf.printf "loading %s\n" input_filename;
  
  (* parse *)
  let program_ast = match parse_file input_filename with
    | Ok ast -> ast
    | Error err_msg ->
        Printf.eprintf "error: %s\n" err_msg;
        exit 1
  in
  
  Printf.printf "parsed\n";
  
  (* type check *)
  (match Typecheck.typecheck_program program_ast with
   | Ok _ -> Printf.printf "type checked\n"
   | Error type_err_msg ->
       Printf.eprintf "type error: %s\n" type_err_msg;
       exit 1);
  
  (* build graph *)
  let dependency_graph = Graph.build_graph program_ast in
  let evaluation_order = Topo.topological_sort dependency_graph in
  
  Printf.printf "built DAG (%d nodes, %d sources)\n"
    dependency_graph.node_count (List.length dependency_graph.sources);
  
  (* run simulation demo *)
  let sim = Runtime_driver.run_demo program_ast in
  
  Printf.printf "\n";
  Printf.printf "execution simulation\n";
  
  (* scenario: robot approaches obstacle *)
  Printf.printf "\nscenario: robot approaches obstacle\n";
  
  (* far away *)
  Runtime_driver.publish sim "distance" (Value.VFloat 2.0);
  Runtime_driver.print_state sim ["distance"; "stop"; "target_speed"; "status"];
  
  (* getting closer *)
  Runtime_driver.publish sim "distance" (Value.VFloat 0.8);
  Runtime_driver.print_state sim ["distance"; "stop"; "target_speed"; "status"];
  
  (* too close! *)
  Runtime_driver.publish sim "distance" (Value.VFloat 0.3);
  Runtime_driver.print_state sim ["distance"; "stop"; "target_speed"; "status"];
  
  (* tick *)
  Runtime_driver.tick sim 20;
  Runtime_driver.print_state sim ["distance"; "stop"; "just_stopped"; "stop_count"; "status"];
  
  (* moving away *)
  Runtime_driver.publish sim "distance" (Value.VFloat 0.7);
  Runtime_driver.print_state sim ["distance"; "stop"; "target_speed"; "status"];
  
  (* another tick *)
  Runtime_driver.tick sim 20;
  Runtime_driver.print_state sim ["stop_count"; "just_stopped"];
  
  (* final state *)
  Printf.printf "\n";
  Runtime_driver.dump_registry sim;
  
  Printf.printf "\nsimulation complete\n"
