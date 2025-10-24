(* signal_registry.ml - stores current and previous values for all signals
   
   this is the core state storage for the runtime
   tracks versions for change detection
*)

open Value

(* entry for each signal *)
type signal_entry = {
  mutable current: value;
  mutable previous: value;
  mutable version: int;  (* bumped on each real change *)
  eq_policy: eq_policy;
  name: string;
}

(* the registry itself *)
type registry = {
  entries: (string, signal_entry) Hashtbl.t;
  mutable current_index: int;  (* current time index *)
}

let create () = {
  entries = Hashtbl.create 128;
  current_index = 0;
}


(* register a new signal *)
let register registry_to_update signal_name initial_val equality_policy =
  if Hashtbl.mem registry_to_update.entries signal_name then
    failwith (Printf.sprintf "Signal '%s' already registered" signal_name);
  
  let new_entry = {
    current = initial_val;
    previous = initial_val;
    version = 0;
    eq_policy = equality_policy;
    name = signal_name;
  } in
  Hashtbl.add registry_to_update.entries signal_name new_entry

let register_with_default registry_to_update signal_name signal_type =
  let default_val = Value.default_value signal_type in
  let policy = Value.default_policy default_val in
  register registry_to_update signal_name default_val policy


(* value access *)
let get registry_to_query signal_name =
  match Hashtbl.find_opt registry_to_query.entries signal_name with
  | Some entry -> entry.current
  | None -> failwith (Printf.sprintf "signal '%s' not found in registry" signal_name)

let get_prev registry_to_query signal_name =
  match Hashtbl.find_opt registry_to_query.entries signal_name with
  | Some entry -> entry.previous
  | None -> failwith (Printf.sprintf "signal '%s' not found in registry" signal_name)

let get_version registry_to_query signal_name =
  match Hashtbl.find_opt registry_to_query.entries signal_name with
  | Some entry -> entry.version
  | None -> 0


(* the publish interface - this is called by adapters when external data arrives
   returns true if this is a real change (version was bumped) *)
let publish registry_to_update signal_name new_val =
  match Hashtbl.find_opt registry_to_update.entries signal_name with
  | None ->
      (* auto-register if not found *)
      let policy = Value.default_policy new_val in
      register registry_to_update signal_name new_val policy;
      true  (* first value is always a change *)
  
  | Some existing_entry ->
      (* check if this is actually different from current *)
      if value_eq existing_entry.eq_policy existing_entry.current new_val then
        false  (* no change - drop it *)
      else begin
        (* real change - update *)
        existing_entry.previous <- existing_entry.current;
        existing_entry.current <- new_val;
        existing_entry.version <- existing_entry.version + 1;
        true
      end


(* direct update for internal use *)
let set registry_to_update signal_name new_value =
  match Hashtbl.find_opt registry_to_update.entries signal_name with
  | Some entry ->
      entry.previous <- entry.current;
      entry.current <- new_value;
      entry.version <- entry.version + 1
  | None ->
      failwith (Printf.sprintf "cannot set unregistered signal '%s'" signal_name)

(* commit all current values as previous - for advancing time index *)
let commit_all registry_to_commit =
  Hashtbl.iter (fun _ entry ->
    entry.previous <- entry.current
  ) registry_to_commit.entries;
  registry_to_commit.current_index <- registry_to_commit.current_index + 1


(* introspection *)
let all_names registry_to_query =
  Hashtbl.fold (fun name _ accumulated -> name :: accumulated) registry_to_query.entries []

let print_registry registry_to_print =
  Printf.printf "\nsignal registry:\n";
  Printf.printf "index: %d\n" registry_to_print.current_index;
  Printf.printf "signals: %d\n\n" (Hashtbl.length registry_to_print.entries);
  
  Printf.printf "%-20s %-10s %-15s %s\n" "name" "version" "current" "previous";
  Printf.printf "%s\n" (String.make 70 '-');
  
  Hashtbl.iter (fun name entry ->
    Printf.printf "%-20s %-10d %-15s %s\n"
      name
      entry.version
      (string_of_value entry.current)
      (string_of_value entry.previous)
  ) registry_to_print.entries;
  Printf.printf "\n"
