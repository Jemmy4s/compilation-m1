(** Directed graphs *)

type ('e, 'n) t

val empty : ('e, 'n) t

type 'n node

val nodes : ('e, 'n) t -> 'n node list

val add_node
  : 'n -> ('e, 'n) t -> 'n node * ('e, 'n) t

val del_node
  : 'n node -> ('e, 'n) t -> ('e, 'n) t

val label_of_node
  : 'n node -> ('e, 'n) t -> 'n

type 'e edge

val add_edge
  : 'e -> 'n node -> 'n node -> ('e, 'n) t -> 'e edge * ('e, 'n) t

val update_edge
  : 'e -> 'n node -> 'n node -> ('e, 'n) t -> ('e, 'n) t

val successors
  : ?p:('e -> bool) -> 'n node -> ('e, 'n) t -> 'n node list

val predecessors
  : ?p:('e -> bool) -> 'n node -> ('e, 'n) t -> 'n node list

val merge
  : 'n node list -> 'n node list -> 'n node list

val show : ?kind:string -> ('e, 'n) t -> ('n -> string) -> ('e -> string) -> unit

