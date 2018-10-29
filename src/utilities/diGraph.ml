(** Directed graphs *)

type 'n node =
    int

type 'e edge =
    int

module IntMap = Map.Make (struct
    type t = int
    let compare = compare
end)

type ('e, 'n) t = {
  counter : int;
  node_labels : 'n IntMap.t;
  edge_labels : 'e IntMap.t;
  successors : ('n node * 'e edge) list IntMap.t;
  predecessors : ('n node * 'e edge) list IntMap.t;
  edges : ('n node * 'n node) IntMap.t;
}

let empty : ('e, 'n) t = {
  counter = 0;
  node_labels = IntMap.empty;
  edge_labels = IntMap.empty;
  successors = IntMap.empty;
  predecessors = IntMap.empty;
  edges = IntMap.empty;
}

let nodes g =
  fst (List.split (IntMap.bindings g.node_labels))

let merge n1 n2 =
  ExtStd.List.uniq (List.sort compare (n1 @ n2))

let add_node label graph =
  let new_node = graph.counter in
  (new_node, { graph with
    counter = graph.counter + 1;
    node_labels = IntMap.add new_node label graph.node_labels;
  })

let del_node n graph =
  let select = List.filter (fun (n', _) -> n' <> n) in
  { graph with
    node_labels  = IntMap.remove n graph.node_labels;
    successors   = IntMap.map select graph.successors;
    predecessors = IntMap.map select graph.predecessors;
    edges =
      IntMap.filter
        (fun _ (n1, n2) -> not (n1 = n || n2 = n))
        graph.edges
  }

let label_of_node n g =
  IntMap.find n g.node_labels

let push k v m =
  IntMap.add k (
    try
      let vs = IntMap.find k m in
      if List.mem v vs then vs else v :: vs
    with Not_found -> [v]
  ) m

let add_edge label start stop graph =
  let new_edge = graph.counter in
  (new_edge, {
    graph with
      counter = graph.counter + 1;
      edge_labels = IntMap.add new_edge label graph.edge_labels;
      successors = push start (stop, new_edge) graph.successors;
      predecessors = push stop (start, new_edge) graph.predecessors;
      edges = IntMap.add new_edge (start, stop) graph.edges;
  })

let edge_between start stop graph =
  List.assoc stop (IntMap.find start graph.successors)

let update_edge label start stop graph =
  try
    let e = edge_between start stop graph in
    let edge_labels = IntMap.add e label graph.edge_labels in
    { graph with edge_labels }
  with Not_found ->
    assert false

let label_of_edge graph e =
  try
    IntMap.find e graph.edge_labels
  with Not_found ->
    assert false

let view what ?(p = fun _ -> true) n graph =
  let all =
    try
      IntMap.find n what
    with Not_found -> []
  in
  List.(map fst (filter (fun (n, e) -> p (label_of_edge graph e)) all))

let successors ?p n graph = view graph.successors ?p n graph

let predecessors ?p n graph = view graph.predecessors ?p n graph

let show ?(kind = "digraph") g node_to_string edge_to_string =
  let dot_node (n, l) =
    Printf.sprintf "n%d [label=\"%s\"];" n (node_to_string l)
  in

  let dot_nodes =
    String.concat "\n" (List.map dot_node (IntMap.bindings g.node_labels))
  in

  let edge_op = match kind with
    | "digraph" -> "->"
    | "graph" -> "--"
    | _ -> "->"
  in
  let seen = Hashtbl.create 13 in

  let canonicalize (x, y) = if x < y then (x, y) else (y, x) in

  let dot_edge (e, l) =
    let start, stop = canonicalize (IntMap.find e g.edges) in
    if kind = "graph" && Hashtbl.mem seen (start, stop) then
      ""
    else (
      Hashtbl.add seen (start, stop) true;
      Printf.sprintf "n%d %s n%d [label=\"%s\"];" start edge_op stop (edge_to_string l)
    )
  in
  let dot_edges =
    String.concat "\n" (List.map dot_edge (IntMap.bindings g.edge_labels))
  in

  let dot =
    Printf.sprintf "%s cfg {\n%s%s\n}"
      kind
      dot_nodes
      dot_edges
  in
  let fname, cout = Filename.open_temp_file "flap" ".dot" in
  output_string cout dot;
  close_out cout;
  Printf.printf "Graph written in %s\n%!" fname
(*  ignore (Sys.command ("rm " ^ fname)); *)
(*  ignore (Sys.command ("dotty " ^ fname ^ "&")) *)
