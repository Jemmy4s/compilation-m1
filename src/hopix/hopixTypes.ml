open HopixAST

(** Type checker error message producer. *)
let error = Error.error "during type checking"

(** Basic types. *)
let tyint  = TyIdentifier (TId "int")
let tybool = TyIdentifier (TId "bool")

(** During typechecking, we thread an environment that contains
    the type of variables and the type definitions. *)
module TypingEnvironment : sig
  (** The type of typing environment. *)
  type t

  (** The empty environment. *)
  val empty : t

  (** [bind env x ty] returns an environment that assigns [ty]
      the variable [x] and extends [env]. *)
  val bind
    : t -> identifier -> typ -> t

  (** [UnboundIdentifier x] is raised if [x] is unbound. *)
  exception UnboundIdentifier of identifier

  (** [lookup env x] returns the type assigned to [x] in [env].
      Raise {!UnboundIdentifier x} if no such variable exists. *)
  val lookup
    : t -> identifier -> typ

  (** [bind_type_definition env t tdef] introduces a new type definition
      in [env]. *)
  val bind_type_definition
    : t -> type_identifier -> type_definition -> t

  (** [UnboundTypeIdentifier t] is raised if no such type [t] exists. *)
  exception UnboundTypeIdentifier of type_identifier

  (** [lookup_type_definition env t] looks for the definition of [t] in
      [env]. May raise {!UnboundTypeIdentifier t} if no such definition
      exists. *)
  val lookup_type_definition
    : t -> type_identifier -> type_definition

  (** [UnboundLabel l] is raised if no record type contains [l] as a label. *)
  exception UnboundLabel of label

  (** [lookup_recordtype_from_label env l] returns the type identifier
      of the record type that contains [l] as well as its definition.
      May raise {!UnboundLabel l}. *)
  val lookup_recordtype_from_label
    : t -> label -> type_identifier * (label * typ) list

  (** This exception is raised when a type identifier is defined but
      it is not a record type. *)
  exception NotRecordType of type_identifier

  (** [lookup_recordtype env t] returns all the field types of
      the record type [t] in [env].
      May raise {!UnboundTypeIdentifier l} or {!NotRecordType t}.*)
  val lookup_recordtype
    : t -> type_identifier -> (label * typ) list

  (** [UnboundTag t] is raised if no such tag [t] exists. *)
  exception UnboundTag of tag

  (** [lookup_tagged_union_type_from_tag env t] returns the type
      identifier of the union type that contains [t] as a tag as well
      as all the types of the tag parameters.
      May raise {!UnboundTag t}. *)
  val lookup_tagged_union_type_from_tag
    : t -> tag -> type_identifier * (tag * typ list) list

  (** This exception is raised if a type identifier is defined but it
      is not a tagged union. *)
  exception NotTaggedUnion of type_identifier

  (** [lookup_tagged_union_typ env t] returns type definition of
      the tagged union type [t] in [env].
      May raise {!UnboundTypeIdentifier t} or {!NotTaggedUnion t}. *)
  val lookup_tagged_union_type
    : t -> type_identifier -> (tag * typ list) list

end = struct
  type t = {
    variables : (identifier * typ) list;
    typedefs  : (type_identifier * type_definition) list;
  }

  let empty = {
    variables = [];
    typedefs  = [];
  }

  let bind e x ty = { e with
    variables = (x, ty) :: e.variables
  }

  exception UnboundIdentifier of identifier

  let lookup e x =
    try
      List.assoc x e.variables
    with Not_found ->
      raise (UnboundIdentifier x)

  let bind_type_definition env t tdef =
    { env with typedefs = (t, tdef) :: env.typedefs }

  exception UnboundTypeIdentifier of type_identifier

  let lookup_type_definition env t =
    try
      List.assoc t env.typedefs
    with Not_found ->
      raise (UnboundTypeIdentifier t)

  exception NotRecordType of type_identifier

  let lookup_recordtype env t =
    match lookup_type_definition env t with
      | RecordTy fs -> fs
      | _ -> raise (NotRecordType t)

  exception UnboundLabel of label

  let lookup_recordtype_from_label env l =
    try
      match List.find (fun (_, tdef) ->
        match tdef with
          | RecordTy fs -> List.exists (fun (l', _) -> l' = l) fs
          | _ -> false
      ) env.typedefs
      with
        | (i, RecordTy fs) -> (i, fs)
        | _ -> assert false (* Because of the predicate below. *)
    with Not_found ->
      raise (UnboundLabel l)

  exception UnboundTag of tag

  let lookup_tagged_union_type_from_tag env t =
    try
      match List.find (fun (_, tdef) ->
        match tdef with
          | TaggedUnionTy ts -> List.exists (fun (t', _) -> t' = t) ts
          | _ -> false
      ) env.typedefs
      with
        | (i, TaggedUnionTy ts) -> (i, ts)
        | _ -> assert false (* Because of the predicate below. *)
    with Not_found ->
      raise (UnboundTag t)

  exception NotTaggedUnion of type_identifier

  let lookup_tagged_union_type env t =
    match lookup_type_definition env t with
      | TaggedUnionTy ks -> ks
      | _ -> raise (NotTaggedUnion t)

end

type typing_environment = TypingEnvironment.t

(** The initial environment contains the type of the primitive functions. *)
let initial_typing_environment () =
     failwith "Student! This is your job!"
