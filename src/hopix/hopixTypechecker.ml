(** This module implements a type checker for Datix. *)
open HopixAST
open HopixTypes

let initial_typing_environment = HopixTypes.initial_typing_environment
type typing_environment = HopixTypes.typing_environment

(** [typecheck tenv ast] checks that [ast] is a well-formed program
    under the typing environment [tenv]. *)
let typecheck tenv ast =

  let rec program tenv p =
    List.fold_left definition tenv p

  and definition tenv def =
    match Position.value def with
      | DefineValue (p, e) ->
        define_value tenv p e

      | DefineType (t, tdef) ->
        let tenv' = TypingEnvironment.bind_type_definition tenv t (TaggedUnionTy []) in
        well_formed_type_definition (Position.position def) tenv' tdef;
        TypingEnvironment.bind_type_definition tenv t tdef

  and well_formed_type_definition pos tenv = function
    | RecordTy ltys ->
         failwith "Student! This is your job!"

    | TaggedUnionTy ktys ->
         failwith "Student! This is your job!"


  (** [define_value tenv p e] returns a new environment that associates
      a type to each of the variables bound by the pattern [p]. *)
  and define_value tenv p e =
       failwith "Student! This is your job!"

  (** [infer_expression_type tenv e] returns the type of the expression
      [e] under the environment [tenv] if [e] is well-typed. *)
  and infer_expression_type tenv e =
    HopixTypeInferenceEngine.infer_expression_type tenv e

  (** [check_pattern tenv pty pat] checks that [pat] can be assigned
      the type [pty] and, if so, returns an extension of [tenv] with
      the variables of [pat] bound to their respective types. *)
  and check_pattern tenv pty pat =
    match Position.value pat, pty with
      | PVariable x, _ ->
           failwith "Student! This is your job!"

      | PTuple xs, TyTuple tys ->
           failwith "Student! This is your job!"

      | PWildcard, _ ->
           failwith "Student! This is your job!"

      | PTaggedValues (k, xs), TyIdentifier t ->
           failwith "Student! This is your job!"

      | _, _ ->
        error (Position.position pat) (
          Printf.sprintf "This pattern has not type: %s\n"
            (HopixPrettyPrinter.(to_string typ pty))
        )

  and check_irrefutable_pattern tenv pat : unit =
    match (Position.value pat) with
      | PWildcard | PVariable _ | PTuple _ -> ()
      | PTaggedValues (k, _) ->
        let t', ktys = TypingEnvironment.lookup_tagged_union_type_from_tag tenv k in
        if List.length ktys <> 1 then
          error (Position.position pat) "This pattern is not irrefutable."

  and check_variable tenv ty x =
    TypingEnvironment.bind tenv x ty

  and check_same_length : type a b. Position.t -> a list -> b list -> unit = fun pos a b ->
    let aln = List.length a and bln = List.length b in
    if (aln <> bln) then (
      error pos
        (Printf.sprintf
           "Invalid number of arguments.\nExpected: %d\nGiven: %d\n."
           aln bln
        )
    )

  and check_unicity
      : type a b. Position.t -> (a * b) list -> string -> unit = fun pos ls what ->
        let ls = List.(sort (fun (l1, _) (l2, _) -> compare l1 l2) ls) in
        let ls = fst (List.split ls) in
        if not (ExtStd.List.all_distinct ls) then
          error pos (Printf.sprintf "Each %s must appear exactly once." what)

  in
  program tenv ast
