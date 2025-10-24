(* topo.ml - topological sort using kahn's algorithm
   
   sorts the DAG so we evaluate parents before children
   this is what prevents glitches in the reactive system
*)

open Graph

exception Cycle of string

(* kahn's algorithm for topological sorting *)
let topological_sort dag =
  (* build adjacency lists for forward traversal *)
  let dependent_nodes = Array.make dag.node_count [] in
  let incoming_edge_count = Array.make dag.node_count 0 in
  
  (* compute in-degrees and reverse edges *)
  Array.iter (fun current_node ->
    List.iter (fun dependency_id ->
      dependent_nodes.(dependency_id) <- current_node.id :: dependent_nodes.(dependency_id);
      incoming_edge_count.(current_node.id) <- incoming_edge_count.(current_node.id) + 1
    ) current_node.deps
  ) dag.nodes;
  
  (* queue of nodes with no dependencies *)
  let ready_queue = Queue.create () in
  for i = 0 to dag.node_count - 1 do
    if incoming_edge_count.(i) = 0 then
      Queue.add i ready_queue
  done;
  
  (* process the queue *)
  let sorted_result = ref [] in
  while not (Queue.is_empty ready_queue) do
    let current_id = Queue.take ready_queue in
    sorted_result := current_id :: !sorted_result;
    
    (* decrease in-degree of dependents *)
    List.iter (fun dependent_id ->
      incoming_edge_count.(dependent_id) <- incoming_edge_count.(dependent_id) - 1;
      if incoming_edge_count.(dependent_id) = 0 then
        Queue.add dependent_id ready_queue
    ) dependent_nodes.(current_id)
  done;
  
  (* check if all nodes were processed - if not we have a cycle *)
  if List.length !sorted_result <> dag.node_count then begin
    (* find the nodes involved in the cycle *)
    let nodes_in_cycle = ref [] in
    for i = 0 to dag.node_count - 1 do
      if incoming_edge_count.(i) > 0 then
        nodes_in_cycle := i :: !nodes_in_cycle
    done;
    let cycle_node_names = List.map (fun id -> node_name dag.nodes.(id)) !nodes_in_cycle in
    raise (Cycle (Printf.sprintf
      "Dependency cycle detected involving: %s"
      (String.concat ", " cycle_node_names)))
  end;
  
  List.rev !sorted_result


(* dependency analysis helpers *)

(* check if node A depends on node B transitively *)
let depends_on dag node_a_id node_b_id =
  let visited_nodes = Hashtbl.create dag.node_count in
  let rec visit current_id =
    if current_id = node_b_id then true
    else if Hashtbl.mem visited_nodes current_id then false
    else begin
      Hashtbl.add visited_nodes current_id ();
      match get_node dag current_id with
      | Some current_node -> List.exists visit current_node.deps
      | None -> false
    end
  in
  visit node_a_id

(* get all ancestors (transitive dependencies) *)
let get_ancestors dag starting_id =
  let visited_set = Hashtbl.create dag.node_count in
  let rec visit current_id accumulated_ancestors =
    if Hashtbl.mem visited_set current_id then accumulated_ancestors
    else begin
      Hashtbl.add visited_set current_id ();
      match get_node dag current_id with
      | Some current_node ->
          List.fold_left (fun acc dep_id -> visit dep_id acc) (current_id :: accumulated_ancestors) current_node.deps
      | None -> accumulated_ancestors
    end
  in
  List.filter ((<>) starting_id) (visit starting_id [])


(* pretty printing *)
let print_topo_order dag sorted_ids =
  Printf.printf "\ntopological order:\n";
  Printf.printf "evaluation order (parents first):\n\n";
  List.iteri (fun index node_id ->
    match get_node dag node_id with
    | Some graph_node ->
        Printf.printf "%3d. [%3d] %-20s (deps: %d)\n"
          (index + 1) node_id (node_name graph_node) (List.length graph_node.deps)
    | None ->
        Printf.printf "%3d. [%3d] <unknown>\n" (index + 1) node_id
  ) sorted_ids;
  Printf.printf "\nTotal: %d nodes\n" (List.length sorted_ids)
