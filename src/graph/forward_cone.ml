(* forward_cone.ml - computes which nodes need re-evaluation
   
   when sources S_i change at index i, we compute the forward
   cone R_i = all nodes reachable from S_i
   
   this is the key optimization - only recompute what's affected
*)

open Graph

module IntSet = Set.Make(Int)

(* build reverse adjacency (parent -> children) for forward traversal *)
let build_children dag =
  let children_lists = Array.make dag.node_count [] in
  Array.iter (fun current_node ->
    List.iter (fun parent_id ->
      children_lists.(parent_id) <- current_node.id :: children_lists.(parent_id)
    ) current_node.deps
  ) dag.nodes;
  children_lists

(* compute forward cone from a set of source nodes using BFS *)
let forward_cone dag source_node_ids =
  let child_adjacency = build_children dag in
  let visited_set = Hashtbl.create dag.node_count in
  let reachable_nodes = ref IntSet.empty in
  
  (* BFS from all sources *)
  let search_queue = Queue.create () in
  List.iter (fun source_id ->
    if not (Hashtbl.mem visited_set source_id) then begin
      Queue.add source_id search_queue;
      Hashtbl.add visited_set source_id ()
    end
  ) source_node_ids;
  
  while not (Queue.is_empty search_queue) do
    let current_id = Queue.take search_queue in
    reachable_nodes := IntSet.add current_id !reachable_nodes;
    
    (* add all children to queue *)
    List.iter (fun child_id ->
      if not (Hashtbl.mem visited_set child_id) then begin
        Queue.add child_id search_queue;
        Hashtbl.add visited_set child_id ()
      end
    ) child_adjacency.(current_id)
  done;
  
  IntSet.elements !reachable_nodes

(* compute forward cone from named sources instead of IDs *)
let forward_cone_names dag source_names =
  let source_ids = List.filter_map (fun name ->
    Hashtbl.find_opt dag.name_to_id name
  ) source_names in
  forward_cone dag source_ids


(* analyze cone sizes for each source *)
let analyze_cones dag =
  Printf.printf "\nforward cone analysis:\n\n";
  Printf.printf "%-20s %-10s %-10s %s\n" "Source" "Cone Size" "% of Graph" "Affected Nodes";
  Printf.printf "%s\n" (String.make 80 '-');
  
  let total_node_count = dag.node_count in
  
  List.iter (fun source_id ->
    match get_node dag source_id with
    | Some source_node ->
        let cone_node_ids = forward_cone dag [source_id] in
        let cone_size = List.length cone_node_ids in
        let percentage_of_graph = (float_of_int cone_size) /. (float_of_int total_node_count) *. 100.0 in
        let affected_nodes = List.filter ((<>) source_id) cone_node_ids in
        let first_few_affected = List.map (fun id ->
          match get_node dag id with
          | Some affected_node -> node_name affected_node
          | None -> "?"
        ) (List.take 5 affected_nodes) in
        let ellipsis = if List.length affected_nodes > 5 then ", ..." else "" in
        Printf.printf "%-20s %-10d %-10.1f%% %s%s\n"
          (node_name source_node)
          cone_size
          percentage_of_graph
          (String.concat ", " first_few_affected)
          ellipsis
    | None -> ()
  ) dag.sources;
  
  Printf.printf "\nAverage cone size: %.1f nodes\n"
    (let total_cone_sizes = List.fold_left (+) 0 
       (List.map (fun id -> List.length (forward_cone dag [id])) dag.sources)
     in
     (float_of_int total_cone_sizes) /. (float_of_int (List.length dag.sources)))

(* helper for taking first n elements *)
let rec take n lst =
  match n, lst with
  | 0, _ | _, [] -> []
  | n, head :: tail -> head :: take (n - 1) tail

module List = struct
  include List
  let take = take
end


(* export graph to DOT format for visualization *)
let to_dot dag output_filename =
  let output_channel = open_out output_filename in
  Printf.fprintf output_channel "digraph RolangDAG {\n";
  Printf.fprintf output_channel "  rankdir=TB;\n";
  Printf.fprintf output_channel "  node [shape=box, style=rounded];\n\n";
  
  (* write nodes *)
  Array.iter (fun graph_node ->
    let node_color = match graph_node.kind with
      | NSignal _ -> "lightblue"
      | NStream _ -> "lightgreen"
      | NLet _ -> "white"
      | NCell _ -> "lightyellow"
      | NExpr _ -> "lightgray"
    in
    let node_label = match graph_node.kind with
      | NSignal (name, node_type) -> Printf.sprintf "%s\\n(signal %s)" name (Ast.string_of_ty node_type)
      | NStream (name, _) -> Printf.sprintf "%s\\n(stream)" name
      | NLet (name, node_type) -> Printf.sprintf "%s\\n: %s" name (Ast.string_of_ty node_type)
      | NCell (name, node_type) -> Printf.sprintf "%s\\n(cell %s)" name (Ast.string_of_ty node_type)
      | NExpr _ -> Printf.sprintf "expr_%d" graph_node.id
    in
    Printf.fprintf output_channel "  n%d [label=\"%s\", fillcolor=%s, style=filled];\n"
      graph_node.id node_label node_color
  ) dag.nodes;
  
  Printf.fprintf output_channel "\n";
  
  (* write edges *)
  Array.iter (fun graph_node ->
    List.iter (fun parent_id ->
      Printf.fprintf output_channel "  n%d -> n%d;\n" parent_id graph_node.id
    ) graph_node.deps
  ) dag.nodes;
  
  Printf.fprintf output_channel "}\n";
  close_out output_channel;
  Printf.printf "\nGraph exported to %s\n" output_filename;
  Printf.printf "Generate image with: dot -Tpng %s -o graph.png\n" output_filename
