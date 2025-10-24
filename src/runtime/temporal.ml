(* temporal.ml - implements prev, window, and sample operators *)

open Value

(* prev operator is handled directly by signal_registry.get_prev *)

(* window operator - ring buffer implementation *)

type window = {
  buffer: value array;  (* circular buffer *)
  mutable head: int;  (* next write position *)
  mutable size: int;  (* how full it is *)
  capacity: int;
}

let create_window buffer_capacity initial_val =
  {
    buffer = Array.make buffer_capacity initial_val;
    head = 0;
    size = 0;
    capacity = buffer_capacity;
  }

let window_push window_to_update new_value =
  window_to_update.buffer.(window_to_update.head) <- new_value;
  window_to_update.head <- (window_to_update.head + 1) mod window_to_update.capacity;
  if window_to_update.size < window_to_update.capacity then
    window_to_update.size <- window_to_update.size + 1

let window_to_list window_to_read =
  let result_list = ref [] in
  for i = 0 to window_to_read.size - 1 do
    let buffer_index = (window_to_read.head - 1 - i + window_to_read.capacity) mod window_to_read.capacity in
    result_list := window_to_read.buffer.(buffer_index) :: !result_list
  done;
  VList !result_list


(* registry of windows - one per (signal, size) pair *)

module WindowKey = struct
  type t = string * int
  let compare = compare
end

module WindowMap = Map.Make(WindowKey)

type window_registry = {
  mutable windows: window WindowMap.t;
}

let create_window_registry () = {
  windows = WindowMap.empty;
}

let get_or_create_window window_reg signal_name window_size initial_val =
  let lookup_key = (signal_name, window_size) in
  match WindowMap.find_opt lookup_key window_reg.windows with
  | Some existing_window -> existing_window
  | None ->
      let new_window = create_window window_size initial_val in
      window_reg.windows <- WindowMap.add lookup_key new_window window_reg.windows;
      new_window

let update_window window_reg signal_name window_size current_val =
  let window_buffer = get_or_create_window window_reg signal_name window_size current_val in
  window_push window_buffer current_val;
  window_to_list window_buffer


(* sample operator - restricts to stream firing indices *)

type sample_state = {
  mutable last_sampled: value;
  mutable last_index: int;
}

let create_sample_state initial_value = {
  last_sampled = initial_value;
  last_index = -1;
}

(* tracks last sampled value per (signal, stream) pair *)
module SampleMap = Map.Make(struct
  type t = string * string
  let compare = compare
end)

type sample_registry = {
  mutable samples: sample_state SampleMap.t;
}

let create_sample_registry () = {
  samples = SampleMap.empty;
}

let sample_value sample_reg signal_name stream_name current_val current_idx stream_just_fired =
  let sample_key = (signal_name, stream_name) in
  let sample_state = match SampleMap.find_opt sample_key sample_reg.samples with
    | Some existing_state -> existing_state
    | None ->
        let new_state = create_sample_state current_val in
        sample_reg.samples <- SampleMap.add sample_key new_state sample_reg.samples;
        new_state
  in
  
  if stream_just_fired && current_idx > sample_state.last_index then begin
    sample_state.last_sampled <- current_val;
    sample_state.last_index <- current_idx
  end;
  
  sample_state.last_sampled
