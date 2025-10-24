(* unix_io.ml - unix I/O using select
   
   portable event loop using Unix.select
   works on macOS, Linux, BSD
*)

open Value

(* platform detection *)
let platform_name =
  let ic = Unix.open_process_in "uname -s" in
  let os_name = input_line ic in
  let _ = Unix.close_process_in ic in
  os_name

let () =
  Printf.printf "(Platform: %s - Using Unix.select)\n" platform_name


(* source definition - something we read from *)
type decoder = bytes -> value option

type source = {
  name: string;
  fd: Unix.file_descr;
  decode: decoder;
  mutable line_buffer: string;
}

(* timer for periodic wakeup *)
type timer = {
  name: string;
  period_ms: int;
  mutable last_fire: float;
}

(* event loop state *)
type event_loop = {
  sources: (string, source) Hashtbl.t;
  timers: (string, timer) Hashtbl.t;
  mutable running: bool;
}

type event =
  | SourceReady of string * value
  | TimerFired of string

let create_event_loop () = {
  sources = Hashtbl.create 32;
  timers = Hashtbl.create 16;
  running = true;
}

let add_source event_loop (src : source) =
  Hashtbl.add event_loop.sources src.name src;
  Unix.set_nonblock src.fd

let add_timer event_loop timer_name period_ms =
  let new_timer = { name = timer_name; period_ms; last_fire = Unix.gettimeofday () } in
  Hashtbl.add event_loop.timers timer_name new_timer

let stop event_loop =
  event_loop.running <- false


(* decoders for different types *)
let decode_float_line bytes =
  try Some (VFloat (float_of_string (String.trim (Bytes.to_string bytes))))
  with _ -> None

let decode_int_line bytes =
  try Some (VInt (int_of_string (String.trim (Bytes.to_string bytes))))
  with _ -> None

let decode_string_line bytes =
  Some (VString (String.trim (Bytes.to_string bytes)))


(* read from file descriptor *)
let read_line_from_fd file_descriptor =
  let byte_buffer = Bytes.create 1 in
  let rec read_chars accumulated_string =
    try
      let bytes_read = Unix.read file_descriptor byte_buffer 0 1 in
      if bytes_read = 0 then None
      else if Bytes.get byte_buffer 0 = '\n' then
        Some (Bytes.of_string accumulated_string)
      else
        read_chars (accumulated_string ^ Bytes.to_string byte_buffer)
    with
    | Unix.Unix_error (Unix.EAGAIN, _, _) -> None
    | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> None
  in
  read_chars ""

let read_and_decode_source source_to_read =
  match read_line_from_fd source_to_read.fd with
  | None -> None
  | Some line_bytes -> source_to_read.decode line_bytes


(* wait for events using Unix.select *)
let wait_for_events event_loop timeout_ms =
  let current_time = Unix.gettimeofday () in
  let fired_events = ref [] in
  
  (* check timers first *)
  Hashtbl.iter (fun timer_name timer_state ->
    let period_seconds = (float_of_int timer_state.period_ms) /. 1000.0 in
    if current_time -. timer_state.last_fire >= period_seconds then begin
      timer_state.last_fire <- current_time;
      fired_events := TimerFired timer_name :: !fired_events
    end
  ) event_loop.timers;
  
  (* return timer events immediately *)
  if !fired_events <> [] then !fired_events
  else begin
    (* get all source file descriptors *)
    let source_fds = Hashtbl.fold (fun _ src acc -> src.fd :: acc) event_loop.sources [] in
    
    if source_fds = [] then begin
      Unix.sleepf ((float_of_int timeout_ms) /. 1000.0);
      []
    end else begin
      let timeout_seconds = (float_of_int timeout_ms) /. 1000.0 in
      let (ready_fds, _, _) = Unix.select source_fds [] [] timeout_seconds in
      
      (* read from each ready fd *)
      List.iter (fun ready_fd ->
        Hashtbl.iter (fun source_name source_state ->
          if source_state.fd = ready_fd then
            match read_and_decode_source source_state with
            | Some decoded_value -> fired_events := SourceReady (source_name, decoded_value) :: !fired_events
            | None -> ()
        ) event_loop.sources
      ) ready_fds;
      
      !fired_events
    end
  end


(* convenience constructors for stdin sources *)
let stdin_source signal_name decoder_func =
  { name = signal_name; fd = Unix.stdin; decode = decoder_func; line_buffer = "" }

let stdin_float signal_name = stdin_source signal_name decode_float_line
let stdin_int signal_name = stdin_source signal_name decode_int_line
let stdin_string signal_name = stdin_source signal_name decode_string_line
