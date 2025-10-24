(* os_bridge.ml - platform abstraction for I/O multiplexing
   
   uses Unix.select as fallback - could add kqueue/epoll later
*)

(* platform detection *)
type platform =
  | Darwin
  | Linux
  | BSD
  | Unknown

let detect_platform () =
  let uname_process = Unix.open_process_in "uname -s" in
  let os_name = input_line uname_process in
  let _ = Unix.close_process_in uname_process in
  match String.lowercase_ascii (String.trim os_name) with
  | "darwin" -> Darwin
  | "linux" -> Linux
  | "freebsd" | "openbsd" | "netbsd" -> BSD
  | _ -> Unknown

let current_platform = detect_platform ()

type fd = Unix.file_descr

type fd_event =
  | Readable
  | Writable
  | Error


(* using select for now - simpler and portable *)
type select_multiplexer = {
  mutable read_fds: (fd * string) list;
  mutable timers: (int * string * float) list;  (* period_ms, name, last_fire *)
}

let create_select_mux () = {
  read_fds = [];
  timers = [];
}

let select_add_fd multiplexer file_descriptor source_name =
  multiplexer.read_fds <- (file_descriptor, source_name) :: multiplexer.read_fds

let select_add_timer multiplexer period_ms timer_name =
  multiplexer.timers <- (period_ms, timer_name, Unix.gettimeofday ()) :: multiplexer.timers

let select_wait multiplexer timeout_ms =
  let current_time = Unix.gettimeofday () in
  
  (* check timers *)
  let fired_timers = List.filter_map (fun (period_ms, timer_name, last_fire_time) ->
    let period_seconds = (float_of_int period_ms) /. 1000.0 in
    if current_time -. last_fire_time >= period_seconds then
      Some (timer_name, Readable)
    else
      None
  ) multiplexer.timers in
  
  (* update timer last_fire times *)
  multiplexer.timers <- List.map (fun (period_ms, timer_name, last_fire_time) ->
    let period_seconds = (float_of_int period_ms) /. 1000.0 in
    if current_time -. last_fire_time >= period_seconds then
      (period_ms, timer_name, current_time)
    else
      (period_ms, timer_name, last_fire_time)
  ) multiplexer.timers;
  
  if fired_timers <> [] then
    fired_timers
  else begin
    (* wait for readable fds *)
    let fd_list = List.map fst multiplexer.read_fds in
    if fd_list = [] then begin
      Unix.sleepf ((float_of_int timeout_ms) /. 1000.0);
      []
    end else begin
      let timeout_seconds = (float_of_int timeout_ms) /. 1000.0 in
      let (ready_fds, _, _) = Unix.select fd_list [] [] timeout_seconds in
      List.filter_map (fun ready_fd ->
        let source_name = List.assoc ready_fd multiplexer.read_fds in
        Some (source_name, Readable)
      ) ready_fds
    end
  end


(* event loop *)
type event_loop = {
  mux: select_multiplexer;
  mutable running: bool;
}

let create_event_loop () = {
  mux = create_select_mux ();
  running = false;
}

let register_source event_loop file_descriptor source_name =
  select_add_fd event_loop.mux file_descriptor source_name

let register_timer event_loop period_ms timer_name =
  select_add_timer event_loop.mux period_ms timer_name

let wait_for_events event_loop timeout_ms =
  select_wait event_loop.mux timeout_ms

let stop_event_loop event_loop =
  event_loop.running <- false


(* platform info *)
let platform_name = match current_platform with
  | Darwin -> "macOS (kqueue available)"
  | Linux -> "Linux (epoll available)"
  | BSD -> "BSD (kqueue available)"
  | Unknown -> "Unknown"

let print_platform_info () =
  Printf.printf "Platform: %s\n" platform_name;
  Printf.printf "Multiplexer: Unix.select (fallback)\n";
  Printf.printf "Note: For production, compile with kqueue/epoll C bindings\n"
