(** This module implements a type inference engine for Hopix.

    The type inference engine takes an implicitly-typed program
    as input and generates an explictly-typed version of this
    program, if it exists.

    To do so, we proceed in four steps:

    1. We first translate the implicitly-typed input program
       into an explicitly typed one where every missing type
       annotations is replaced with a "unification type variable",
       that is an unknown type variable.

    2. We generate a typing constraint out of the program obtained
       in the previous step.

    3. We solve the typing constraint using first-order unification.

    4. We then have 3 cases:
       (i) if the typing constraint is unsatisfiable, the input
           program is ill-typed ;
       (ii) if the typing constraint is satisfiable, we have two
            sub-cases:
           (a) the solved form of the constraint assigns a ground
               type to each unification type variable ;
           (b) the solved form of the constraint does not assign
               a ground to each unification type variable.

       Only the subcase (ii)-(b) allows us to generate an explicitly
       typed version of the program. Other cases produce errors and
       stop the compiler.

*)

open HopixTypes
open HopixAST

(** {1 Helpers}. *)

(** [locate f e] rewrites [e] using [f], keeping the locations of [e] in
    [f e]. *)
let locate f e =
  Position.map f e

(** We reuse type identifiers to represent unification type variables.
    As, the program can only define type constructors starting with a
    lowercase letter, the unification variable can be represented by
    using names starting with an uppercases letter. *)
let is_unification_variable (TId x) =
  x.[0] = 'X'

(** [fresh_type_variable ()] returns a new unification type variable. *)
let fresh_unification_type_variable =
  let r = ref 0 in
  fun () -> incr r; TId ("X" ^ string_of_int !r)

(** [exists f] produces the constraint [∃ X. f X]. *)
let exists f =
  let x = fresh_unification_type_variable () in
  f x

(** {1 Step 1: Pre-annotation of the implicitly-typed input program.} *)

(** [preannotate_expression e] puts a fresh unification type variable
    at every place where a type annotation is missing. *)
let rec preannotate_expression e =
   failwith "Students! This is your job!"

and lambda ((a, e) : lambda) =
   failwith "Students! This is your job!"

and sub e = locate preannotate_expression e

and branch (Branch (p, e)) = Branch (p, sub e)

(** {1 Step 2: Typing constraint generation.} *)

(** An atomic constraint is an equality between two types. *)
type aconstraint =
  | CEq of typ * typ

(** [ty1 =?= ty2] is a notation for [CEq (ty1, ty2)]. *)
let ( =?= ) ty1 ty2 = CEq (ty1, ty2)

(** A typing constraint is a conjunction of atomic constraint. *)
type tconstraint = aconstraint list

(** [cconj [c1; c2; ..; cN]] represents [c1 ∧ c2 ∧ … ∧ cN]. *)
let cconj = List.flatten

(** [generate_constraint tenv e ty] produces [xs, c] such that if
    there exist [xs], such that [c] is satisfiable then [e] has type
    [ty] in [tenv]. *)
let rec generate_constraint pos tenv e ty =
  failwith "Student! This is your job!"



and lambda tenv (a, e) ty =
   failwith "Students! This is your job!"

and generate_constraint' tenv e ty =
  generate_constraint (Position.position e) tenv (Position.value e) ty

let well_typedness_constraint pos tenv e =
  let x = fresh_unification_type_variable () in
  try
    (x, generate_constraint pos tenv e (TyIdentifier x))
  with _ ->
    error pos "Type error."

let tconstraint_to_string t =
  let aconstraint = function
    | CEq (a, b) ->
      Printf.sprintf "%s =?= %s"
        (HopixPrettyPrinter.(to_string typ a))
        (HopixPrettyPrinter.(to_string typ b))
  in
  "[\n" ^ String.concat "\n" (List.map aconstraint t) ^ "\n]"


(** {1 Step 3: Solve the typing constraint} *)


module Substitution : sig
(** A (acyclic) substitution is a function φ from type variables
    to types that maintains the following invariants:

    - ∀ x, φ (x) = T if x ∈ domain (φ) and φ (x) = x otherwise.
    - ∀ x, x ∉ TV (φ (x)).

    where TV(T) represents the type variables that appear in the type T. *)
  type substitution
  type t = substitution

  (** The identity substitution. *)
  val identity : substitution

  (** [image φ] returns the image of domain(φ). *)
  val image : substitution -> typ list

  (** [binds φ x t] returns a new substitution φ' such that φ'(x) = t,
      and φ' (y) = φ (y) otherwise. Assume that x ∉ domain(φ) and that
      ∀ y, y ∈ domain (φ), then y ∉ TV(t). *)
  val binds : substitution -> type_identifier -> typ -> substitution

  (** [InvalidBinding] is raised if the precondition of [binds] is not
      respected.  *)
  exception InvalidBinding

  (** [lookup φ x] returns [φ (x)] *)
  val lookup : substitution -> type_identifier -> typ

  (** [apply φ ty] returns the type ty in which every type variables [x] in
      domain(φ) is replaced by [φ (x)]. *)
  val apply : substitution -> typ -> typ

  (** [to_string φ] is the string representation of the substitution φ. *)
  val to_string : substitution -> string
end = struct
  (** A naive implementation of the substitution. Please feel free to optimize it! *)
  type substitution = (type_identifier * typ) list
  type t = substitution

  let identity = []

  let image phi = snd (List.split phi)

  let in_domain phi x =
    List.exists (fun (y, _) -> x = y) phi

  exception InvalidBinding

  let binds phi x ty =
    if (in_domain phi x) then raise InvalidBinding
    else begin
      let rec occurs_check = function
        | TyIdentifier y -> if x = y then raise InvalidBinding
        | TyTuple ts -> List.iter occurs_check ts
        | TyArrow (ity, oty) -> occurs_check ity; occurs_check oty
      in
      occurs_check ty;
      (x, ty) :: phi
    end

  let lookup phi x =
    try List.assoc x phi with Not_found -> TyIdentifier x

  let to_string phi =
    "[" ^ String.concat "; " (List.map (fun (TId x, ty) ->
      x ^ " ↦ " ^ HopixPrettyPrinter.(to_string typ ty)
    ) phi) ^ "]"

  let apply phi x =
    let rec apply ty =
      match ty with
        | TyIdentifier x ->
          let ty' = lookup phi x in
          if ty <> ty' then apply ty' else ty
        | TyTuple ts ->
          TyTuple (List.map apply ts)
        | TyArrow (ity, oty) ->
          TyArrow (apply ity, apply oty)
    in
    apply x

end

exception NotUnifiable

(** [unify c] uses first-order unification to return a substitution φ
    that satisfies [c]. *)
let rec unify = function
    | [] ->
      Substitution.identity

    | CEq (a, b) :: cs ->
         failwith "Student! This is your job!"

type answer =
  | Unsatisfiable
  | Satisfiable of Substitution.t
  | NotConstrainedEnough of Substitution.t

let answer_to_string = function
  | Unsatisfiable ->
    "Unsatisfiable"
  | Satisfiable phi ->
    "Satisfiable with "
    ^ Substitution.to_string phi
  | NotConstrainedEnough phi ->
    "Satisfiable with "
    ^ Substitution.to_string phi
    ^ " but not constrained enough."

(** [solve c] returns an answer to the satisfiability of [c] seen as a
    typing constraint for an Hopix program. *)
let rec solve c =
  try
    let phi = unify c in
    if ground_solved_form phi then
      Satisfiable phi
    else
      NotConstrainedEnough phi
  with e ->
    Unsatisfiable

(** [ground_typ ty] returns [true] if and only if [ty] does not
    contain unification variables. *)
and ground_typ = function
  | TyIdentifier x when is_unification_variable x -> false
  | TyArrow (a, b) -> ground_typ a && ground_typ b
  | TyTuple ts -> List.for_all ground_typ ts
  | TyIdentifier x -> true

(** [ground_solved_form phi] returns [true] if and only if
    [phi] only contains ground types in its image. *)
and ground_solved_form phi =
  List.(for_all
          (fun ty -> ground_typ (Substitution.apply phi ty))
          (Substitution.image phi))

(** {1 Elaborate} *)
let elaborate phi e =
  let rec aux e =
       failwith "Students! This is your job!"

  and branch (Branch (p, e)) =
    Branch (p, sub e)

  and lambda (a, e) =
     failwith "Students! This is your job!"
  and sub e = locate aux e
  in
  aux e

(** {1 Glue everything together.} *)

let infer_expression_type tenv e =
  let e = Position.value e and pos = Position.position e in
  let e' = preannotate_expression e in
  let x, c = well_typedness_constraint pos tenv e' in
  let a = solve c in
  if Options.get_verbose_mode () then
    Printf.eprintf "\
       ** Pre-annotated program:\n%s\n\
       ** Typing constraint:\n%s\n\
       ** Solver answer:\n%s\n"
      (HopixPrettyPrinter.(to_string expression e'))
      (tconstraint_to_string c)
      (answer_to_string a);

  match a with
    | Satisfiable phi ->
      let e'' = elaborate phi e'
      and ety = Substitution.(apply phi (TyIdentifier x)) in
      if Options.get_verbose_mode () then
        Printf.eprintf "\
          ** Explicitly-typed program:\n%s\n\
          ** Its type:\n%s\n%!"
          (HopixPrettyPrinter.(to_string expression e''))
          (HopixPrettyPrinter.(to_string typ ety));
      ety

    | NotConstrainedEnough phi ->
      let e'' = elaborate phi e'
      and ety = Substitution.(lookup phi x) in
      if Options.get_verbose_mode () then
        Printf.eprintf "\
       ** Explicitly-typed program:\n%s\n\
       ** Its type:\n%s\n%!"
          (HopixPrettyPrinter.(to_string expression e''))
          (HopixPrettyPrinter.(to_string typ ety));
      error pos "Not enough type annotations to infer the type of this expression."

    | Unsatisfiable ->
      error pos "Type error."
