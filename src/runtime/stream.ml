(* stream.ml - executes periodic stream blocks with local state
   
   streams fire at their declared cadence and maintain local state
   between firings using prev
*)

open Ast
open Value

(* stream runtime state *)
type stream_state = {
  name: string;
  def: stream_def;
  
  (* local state cells *)
  mutable state_values: (string, value) Hashtbl.t;
  mutable prev_state_values: (string, value) Hashtbl.t;
  
  cadence_ms: int option;
  
  (* tracking when stream fires *)
  mutable last_fired_index: int;
  mutable fire_count: int;
}

let create_stream stream_name stream_definition cadence_milliseconds =
  {
    name = stream_name;
    def = stream_definition;
    state_values = Hashtbl.create 16;
    prev_state_values = Hashtbl.create 16;
    cadence_ms = cadence_milliseconds;
    last_fired_index = -1;
    fire_count = 0;
  }


(* initialize stream state from start bindings *)
let initialize_stream stream_to_init eval_expr =
  List.iter (fun (start_pattern, init_expression) ->
    let init_value = eval_expr init_expression in
    match start_pattern with
    | PVar variable_name ->
        Hashtbl.add stream_to_init.state_values variable_name init_value;
        Hashtbl.add stream_to_init.prev_state_values variable_name init_value
    | _ -> failwith "Complex patterns in stream start not yet supported"
  ) stream_to_init.def.stream_state


(* execute one firing of the stream block
   returns emitted value if any *)
let execute_stream stream_to_run eval_expr current_index =
  (* update firing tracking *)
  stream_to_run.last_fired_index <- current_index;
  stream_to_run.fire_count <- stream_to_run.fire_count + 1;
  
  (* commit current state to previous *)
  Hashtbl.clear stream_to_run.prev_state_values;
  Hashtbl.iter (fun state_var current_val ->
    Hashtbl.add stream_to_run.prev_state_values state_var current_val
  ) stream_to_run.state_values;
  
  (* evaluate updates - can use prev state *)
  List.iter (fun (update_pattern, update_expression) ->
    let new_value = eval_expr update_expression in
    match update_pattern with
    | PVar state_var_name ->
        Hashtbl.replace stream_to_run.state_values state_var_name new_value
    | _ -> failwith "Complex patterns in stream update not yet supported"
  ) stream_to_run.def.stream_updates;
  
  (* evaluate emit expression *)
  match stream_to_run.def.stream_emit with
  | Some emit_expression -> Some (eval_expr emit_expression)
  | None -> None


(* stream registry - tracks all active streams *)

type stream_registry = {
  streams: (string, stream_state) Hashtbl.t;
}

let create_stream_registry () = {
  streams = Hashtbl.create 32;
}

let register_stream stream_reg stream_name stream_def cadence_ms =
  let new_stream = create_stream stream_name stream_def cadence_ms in
  Hashtbl.add stream_reg.streams stream_name new_stream;
  new_stream

let find_stream stream_reg stream_name =
  Hashtbl.find_opt stream_reg.streams stream_name

let all_streams stream_reg =
  Hashtbl.fold (fun _ stream_state acc -> stream_state :: acc) stream_reg.streams []

(* figure out which streams should fire at current time *)
let streams_to_fire stream_reg current_time_ms =
  Hashtbl.fold (fun stream_name stream_state accumulated_names ->
    match stream_state.cadence_ms with
    | None -> accumulated_names
    | Some period_ms ->
        (* simple check - fire if period elapsed *)
        if current_time_ms mod period_ms = 0 then
          stream_name :: accumulated_names
        else
          accumulated_names
  ) stream_reg.streams []


(* debugging *)
let print_stream_state stream_to_print =
  Printf.printf "\nStream: %s\n" stream_to_print.name;
  Printf.printf "  Fires: %d times\n" stream_to_print.fire_count;
  Printf.printf "  Last: index %d\n" stream_to_print.last_fired_index;
  Printf.printf "  State:\n";
  Hashtbl.iter (fun var_name var_value ->
    Printf.printf "    %s = %s\n" var_name (string_of_value var_value)
  ) stream_to_print.state_values
