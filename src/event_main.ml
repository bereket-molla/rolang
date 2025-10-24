(* event_main.ml - event-driven execution with real Unix I/O *)

open Lexing

let parse_and_check input_filename =
  let input_channel = open_in input_filename in
  let lexer_buffer = Lexing.from_channel input_channel in
  lexer_buffer.lex_curr_p <- { lexer_buffer.lex_curr_p with pos_fname = input_filename };
  
  let parsed_ast = Parser.program Scanner.token lexer_buffer in
  close_in input_channel;
  
  (match Typecheck.typecheck_program parsed_ast with
   | Ok _ -> ()
   | Error type_error_msg ->
       Printf.eprintf "Type error: %s\n" type_error_msg;
       exit 1);
  
  parsed_ast

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <file.rol> [--verbose]\n" Sys.argv.(0);
    Printf.eprintf "\nEvent-driven runtime. Reads signal values from stdin.\n";
    Printf.eprintf "Example: echo \"2.5\" | %s examples/demo.rol\n" Sys.argv.(0);
    exit 1
  end;
  
  let input_filename = Sys.argv.(1) in
  let verbose_mode = Array.length Sys.argv > 2 && Sys.argv.(2) = "--verbose" in
  
  Printf.printf "rolang event runtime\n\n";
  
  Printf.printf "loading %s\n" input_filename;
  
  let program_ast = parse_and_check input_filename in
  Printf.printf "parsed and type-checked\n";
  
  let dependency_graph = Graph.build_graph program_ast in
  let evaluation_order = Topo.topological_sort dependency_graph in
  Printf.printf "built DAG (%d nodes, %d sources)\n\n"
    dependency_graph.node_count (List.length dependency_graph.sources);
  
  let runtime = Event_runtime.create dependency_graph evaluation_order verbose_mode in
  
  (* configure sources based on signal declarations *)
  Printf.printf "----- source configuration ----- \n";
  Array.iter (fun graph_node ->
    match graph_node.Graph.kind with
    | Graph.NSignal (signal_name, signal_type) ->
        (match signal_type with
         | Ast.TyBase Ast.TFloat ->
             Printf.printf "  %s: float (stdin line)\n" signal_name;
             let float_source = Unix_io.stdin_float signal_name in
             Event_runtime.register_source runtime float_source
         | Ast.TyBase Ast.TInt ->
             Printf.printf "  %s: int (stdin line)\n" signal_name;
             let int_source = Unix_io.stdin_int signal_name in
             Event_runtime.register_source runtime int_source
         | Ast.TyBase Ast.TString ->
             Printf.printf "  %s: string (stdin line)\n" signal_name;
             let string_source = Unix_io.stdin_string signal_name in
             Event_runtime.register_source runtime string_source
         | _ ->
             Printf.printf "  %s: %s (no auto-config)\n" signal_name (Ast.string_of_ty signal_type))
    | _ -> ()
  ) dependency_graph.Graph.nodes;
  
  (* configure timers *)
  Printf.printf "\n ----- timer configuration ----- \n";
  Array.iter (fun graph_node ->
    match graph_node.Graph.kind with
    | Graph.NStream (stream_name, stream_def) ->
        (match stream_def.Ast.stream_cadence with
         | Some _cadence_name ->
             (* TODO: lookup actual period *)
             let default_period_ms = 20 in
             Printf.printf "  %s: every %dms\n" stream_name default_period_ms;
             Event_runtime.register_timer runtime stream_name default_period_ms
         | None ->
             Printf.printf "  %s: no cadence\n" stream_name)
    | _ -> ()
  ) dependency_graph.Graph.nodes;
  
  Printf.printf "\nready - type signal values (one per line)\n";
  Printf.printf "ctrl-D to stop, ctrl-C to interrupt\n\n";
  
  (* run event loop *)
  (try
    Event_runtime.run runtime 100
  with
  | Unix.Unix_error (unix_err, func_name, arg) ->
      Printf.printf "\nUnix error in %s: %s (%s)\n" func_name (Unix.error_message unix_err) arg;
      exit 1
  | End_of_file ->
      Printf.printf "\nEnd of input\n"
  | Sys.Break ->
      Printf.printf "\nInterrupted by user\n");
  
  Printf.printf "\n";
  Event_runtime.dump_state runtime;
  
  Printf.printf "\ndone\n"
