(* event_runtime.ml - integrates unix I/O with evaluator *)

open Value
open Graph

type runtime = {
  eval_env: Evaluator.eval_env;
  event_loop: Unix_io.event_loop;
  mutable current_time_ms: int;
  verbose: bool;
}

let create dependency_graph topological_order verbose_mode =
  let evaluation_environment = Evaluator.create_env dependency_graph topological_order in
  Evaluator.initialize_from_graph evaluation_environment;
  {
    eval_env = evaluation_environment;
    event_loop = Unix_io.create_event_loop ();
    current_time_ms = 0;
    verbose = verbose_mode;
  }

let register_source runtime_state source_to_register =
  Unix_io.add_source runtime_state.event_loop source_to_register

let register_timer runtime_state timer_name period_ms =
  Unix_io.add_timer runtime_state.event_loop timer_name period_ms

let process_event runtime_state incoming_event =
  match incoming_event with
  | Unix_io.SourceReady (signal_name, new_value) ->
      if runtime_state.verbose then
        Printf.printf "%s = %s\n" signal_name (string_of_value new_value);
      
      if Signal_registry.publish runtime_state.eval_env.registry signal_name new_value then begin
        Evaluator.eval_forward_cone runtime_state.eval_env [signal_name];
        Signal_registry.commit_all runtime_state.eval_env.registry
      end
  
  | Unix_io.TimerFired timer_name ->
      if runtime_state.verbose then
        Printf.printf "timer %s\n" timer_name;
      
      Evaluator.eval_forward_cone runtime_state.eval_env [timer_name];
      Signal_registry.commit_all runtime_state.eval_env.registry

let run runtime_state max_iterations =
  Printf.printf "\nevent loop starting\n\n";
  
  let iteration_count = ref 0 in
  
  while runtime_state.event_loop.running && !iteration_count < max_iterations do
    let events = Unix_io.wait_for_events runtime_state.event_loop 100 in
    List.iter (process_event runtime_state) events;
    incr iteration_count
  done;
  
  if !iteration_count >= max_iterations then
    Printf.printf "\nstopped after %d iterations\n" !iteration_count

let dump_state runtime_state =
  Signal_registry.print_registry runtime_state.eval_env.registry

let get_value runtime_state signal_name =
  Signal_registry.get runtime_state.eval_env.registry signal_name
