(** The abstract syntax tree for hopix programs. *)

open Position

type program = definition located list

and definition =
  (** A toplevel definition for a value. *)
  | DefineValue of pattern located * expression located

  (** A definition for a type. *)
  | DefineType of type_identifier * type_definition

and expression =
  (** A literal is a constant written "as is". *)
  | Literal of literal
  (** A variable is a name for a value. *)
  | Variable of identifier
  (** A local definition [val p = e₁ in e₂ end]. *)
  | Define of pattern located * expression located * expression located
  (** A function application [a b]. *)
  | Apply of expression located * expression located
  (** An anonymous function [{ (x : T) -> e }]. *)
  | Fun of lambda
  (** A tuple of mutually-recursive functions.

      [
        rec f1 x₁₁ ⋯ x₁N₁ : T₁ = e₁
        and ...
        and fM xM₁ ⋯ xM{N_M} : TM = eM
      ]

      let (f1, .., fM) =
         fix f1 x11 ... x1N1 = e1
         and ...
         and fM xM1 ... x... = eM

      is represented as a list of pairs where the
      first component is the name fI of the function
      and the second component is an expression
      representing
      [
        λxI₁ ⋯ xI{N_I} . eI
      ]
  *)
  | RecFuns of (typed_identifier located * expression located)  list
  (** A tuple of values [(e₁, ⋯, eN)] *)
  | Tuple of expression located list
  (** A record is a tuple of labelled values [{ l₁ = e₁; ⋯; lN = eN }]. *)
  | Record of (label * expression located) list
  (** A field extraction [e.l₁]. *)
  | RecordField of expression located * label
  (** A tagged value [K (e₁, ⋯, eN)]. *)
  | TaggedValues of tag * expression located list
  (** A conditional expression [if c then e₁ else e₂ end] *)
  | IfThenElse of expression located * expression located * expression located
  (** A case analysis [case s with p₁ -> e₁ | ⋯ | pN -> eN end] *)
  | Case of expression located * branch list

  (* Only appears in the image of closure conversion. *)
  | MutateTuple of expression located * int * expression located

and lambda = typed_identifier located * expression located

and typed_identifier = identifier * typ option

and tag =
  | Constructor of string

and branch =
  | Branch of pattern located * expression located

and pattern =
  | PWildcard
  | PVariable     of identifier
  | PTuple        of identifier list
  | PTaggedValues of tag * identifier list

and literal =
  | LInt of int

and identifier =
  | Id of string

and typ =
  | TyIdentifier of type_identifier
  | TyTuple      of typ list
  | TyArrow      of typ * typ

and type_definition =
  | RecordTy      of (label * typ) list
  | TaggedUnionTy of (tag * typ list) list

and label =
  | Label of string

and type_identifier =
  | TId of string

and t = program

module VariableSet = Set.Make (struct
    type t = identifier
    let compare (Id s1) (Id s2) = String.compare s1 s2
end)

let seq e1 e2 =
  Position.map (fun _ -> Define (Position.map (fun _ -> PWildcard) e1, e1, e2)) e1

let rec seqs = function
  | [] -> assert false (* By precondition. *)
  | [e] -> e
  | e1 :: e2 -> seq e1 (seqs e2)

let is_binary_primitive = function
  | "+" | "-" | "*" | "/" | "<" | ">" | "<=" | ">=" | "=" -> true
  | _ -> false

let free_variables : expression -> identifier list =
     failwith "Student! This is your job!"
