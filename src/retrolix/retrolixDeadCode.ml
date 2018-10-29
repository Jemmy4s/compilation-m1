(** This module implements an optimization from Retrolix to Retrolix
    to remove dead code. *)

open RetrolixAST

module LabelSet = Set.Make (struct
    type t = label
    let compare = compare
end)

(** [unreachable cfg entries] returns the set of labels that
    are not reachable from the node [entries]. *)
let unreachable cfg entries =
  failwith "Students! This is your work!"
  </sujet. *)

(** [eliminate cfg p] removes all instructions from [p] that are
    statically known to be dead in [p]. *)
let eliminate cfg p =
  failwith "Students! This is your work!"
  </sujet. *)
