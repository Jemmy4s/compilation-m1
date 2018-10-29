(** This module extends some modules of the standard library. *)

module List = struct

  include List

  let rec range start stop =
    if stop < start then [] else start :: range (start + 1) stop

  let asymmetric_map2 f =
    let rec aux accu xs ys =
      match xs, ys with
        | xs, [] ->
          (List.rev accu, xs, [])
        | [], ys ->
          (List.rev accu, [], ys)
        | x :: xs, y :: ys ->
          aux (f x y :: accu) xs ys
    in
    aux []

  let rec uniq = function
    | [] -> []
    | [x] -> [x]
    | x :: ((y :: _) as xs) -> if x = y then uniq xs else x :: uniq xs

  (** [index_of p l] returns the index of the first element [x] of [l]
      such [p x = true]. Raise [Not_found] otherwise. *)
  let index_of : ('a -> bool) -> 'a list -> int =
    fun p l ->
      let rec aux i = function
        | [] -> raise Not_found
        | x :: xs -> if p x then i else aux (succ i) xs
      in
      aux 0 l

   (** [all_distinct ls] returns true if all the elements of [ls]
       are distinct. *)
  let all_distinct ls =
    let ls = List.sort compare ls in
    let rec aux = function
      | [] | [_] -> true
      | x :: y :: ys -> x <> y && aux (y :: ys)
    in
    aux ls

  let foldmap f init =
    let rec aux (accu, ys) = function
      | [] ->
        (accu, List.rev ys)
      | x :: xs ->
        let accu, y = f accu x in
        aux (accu, y :: ys) xs
    in
    aux (init, [])

  module Monad : sig
      type 'a t
      val return : 'a -> 'a t
      val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
      val take_one : 'a list -> 'a t
      val fail : 'a t
      val and_try : 'a t -> 'a t -> 'a t
      val run : 'a t -> 'a list
  end = struct
      type 'a t = 'a list
      let return x = [x]
      let ( >>= ) x f = List.(flatten (map f x))
      let fail = []
      let and_try a b = a @ b
      let run x = x
      let take_one x = x
  end

end
