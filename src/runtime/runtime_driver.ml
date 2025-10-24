(* runtime_driver.ml - simulation mode for testing
   
   lets us manually publish values and tick streams
   for debugging and testing
*)

open Ast
open Value
open Graph

(* simulation state *)
type simulation = {
  env: Evaluator.eval_env;
  mutable current_time_ms: int;
}

let create_simulation dependency_graph topological_order =
  let eval_environment = Evaluator.create_env dependency_graph topological_order in
  Evaluator.initialize_from_graph eval_environment;
  { env = eval_environment; current_time_ms = 0 }


(* publish interface for testing *)
let publish sim signal_name new_value =
  Printf.printf "\n  publish '%s' = %s at index %d\n"
    signal_name (string_of_value new_value) sim.env.registry.current_index;
  
  if Signal_registry.publish sim.env.registry signal_name new_value then begin
    Printf.printf "  -> Change detected (version %d)\n"
      (Signal_registry.get_version sim.env.registry signal_name);
    
    Printf.printf "  -> Computing forward cone...\n";
    Evaluator.eval_forward_cone sim.env [signal_name];
    
    Signal_registry.commit_all sim.env.registry
  end else begin
    Printf.printf "  -> No change (value equals previous)\n"
  end


(* advance time and fire streams *)
let tick sim delta_milliseconds =
  sim.current_time_ms <- sim.current_time_ms + delta_milliseconds;
  Printf.printf "\n Tick +%dms (t=%dms) at index %d\n"
    delta_milliseconds sim.current_time_ms sim.env.registry.current_index;
  
  (* check which streams should fire *)
  let streams_firing = Stream.streams_to_fire sim.env.stream_registry sim.current_time_ms in
  
  if streams_firing = [] then
    Printf.printf "  -> No streams firing\n"
  else begin
    Printf.printf "  -> Firing streams: %s\n" (String.concat ", " streams_firing);
    
    Evaluator.eval_forward_cone sim.env streams_firing;
    
    Signal_registry.commit_all sim.env.registry
  end


(* inspect current state *)
let print_state sim signal_names =
  Printf.printf "\nstate at index %d:\n" sim.env.registry.current_index;
  List.iter (fun signal_name ->
    try
      let current_value = Signal_registry.get sim.env.registry signal_name in
      let version_number = Signal_registry.get_version sim.env.registry signal_name in
      Printf.printf "  %-20s = %-15s (v%d)\n"
        signal_name (string_of_value current_value) version_number
    with _ ->
      Printf.printf "  %-20s = <not found>\n" signal_name
  ) signal_names


(* full registry dump *)
let dump_registry sim =
  Signal_registry.print_registry sim.env.registry


(* demo runner *)
let run_demo program_ast =
  Printf.printf "\nrolang runtime demo\n\n";
  
  let dependency_graph = Graph.build_graph program_ast in
  let evaluation_order = Topo.topological_sort dependency_graph in
  
  let sim = create_simulation dependency_graph evaluation_order in
  
  Printf.printf "runtime initialized with %d nodes\n" dependency_graph.node_count;
  Printf.printf "sources: %d\n" (List.length dependency_graph.sources);
  
  dump_registry sim;
  
  sim
