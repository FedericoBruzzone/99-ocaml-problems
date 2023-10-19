module Node (T : sig type t end) = struct
  type t = T.t
end

module type GraphADT = sig
  type node
  type 'a t
  val empty : 'a t
  val add_node : node -> 'a t -> 'a t
end

module GraphCDT (Node : sig type t end) = struct
  type node = Node.t
  type 'a t = Graph of node list * (node * node) list

  let empty = Graph ([], [])
  let add_node node g = match g with
    | Graph (nodes, edges) -> Graph (node :: nodes, edges)
end

module Graph = GraphCDT (Node (Int))

let () =
  let g = Graph.empty in
  let _ = Graph.add_node 1 g in
  ()

