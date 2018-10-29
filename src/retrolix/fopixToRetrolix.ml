(** This module implements a compiler from Fopix to Retrolix. *)

let error pos msg =
  Error.error "compilation" pos msg

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Fopix
module Target = Retrolix

(** We will need the following pieces of information to be carrying
    along the translation: *)
module IdCmp = struct
  type t = Target.AST.identifier
  let compare = compare
end
module IdSet = Set.Make (IdCmp)
module IdMap = Map.Make (IdCmp)

type environment = unit

(** Initially, the environment is empty. *)
let initial_environment () = ()

let fresh_label =
  let c = ref 0 in
  fun () -> incr c; Target.AST.Label ("l" ^ string_of_int !c)

let fresh_variable =
  let c = ref 0 in
  fun () -> incr c; Target.AST.(Id ("X" ^ string_of_int !c))

let fresh_spilled_variable =
  let c = ref 0 in
  fun () -> incr c; Target.AST.(Id ("S" ^ string_of_int !c))

let is_spilled_variable (Target.AST.Id x) =
  x.[0] = 'S'

let is_local_variable (Target.AST.Id x) =
  x.[0] = 'X'

(* Student! The following function is wrong! You must correct it! *)
let locals env b = Target.AST.(IdSet.(
  let unions = List.fold_left union empty in
  let rec locals (_, i) =
    match i with
      | Call (l, r, rs) ->
        unions ([rvalue (l :> rvalue); rvalue r] @ List.map rvalue rs)
      | Ret r ->
        rvalue r
      | Assign (l, _, rs) ->
        unions (rvalue (l :> rvalue) :: List.map rvalue rs)
      | ConditionalJump (_, rs, _, _) ->
        unions (List.map rvalue rs)
      | _ ->
        empty
  and rvalue = function
    | `Variable x when is_local_variable x -> singleton x
    | _ -> empty
  in
  elements (unions (List.map locals b))
))

(** [translate' p env] turns a Fopix program [p] into a Retrolix program
    using [env] to retrieve contextual information. *)
let rec translate' p env =
  let defs = List.map (declaration env) p in
  (defs, env)

and identifier (Source.AST.Id x) = Target.AST.Id x

and register r =
  Target.AST.((`Register (RId (MipsArch.string_of_register r)) : lvalue))


and declaration env = function
  | Source.AST.DefineValue (x, e) -> Target.AST.(
    let ec = expression' (`Variable (identifier (Position.value x))) e in
    let locals = locals env ec in
    DValue (identifier (Position.value x), (locals, ec))
  )

  | Source.AST.DefineFunction (f, xs, e) ->
    let Source.AST.FunId f = Position.value f in
    Target.AST.(
      let x = fresh_variable () in
      let ec = expression' (`Variable x) e in
      DFunction (FId f,
                 List.map identifier xs,
                 (locals env ec,
                  ec @ [labelled (Ret (`Variable x))]))
    )

(** [expression pos x e] compiles [e] into a block of Retrolix
    instructions that stores the evaluation of [e] into [x]. *)
and expression pos out = Target.AST.(function
  | Source.AST.Literal l ->
    [labelled (Assign (out, Load, [ `Immediate (literal l) ]))]

  | Source.AST.Variable (Source.AST.Id x) ->
    [labelled (Assign (out, Load, [ `Variable (Id x) ]))]

  | Source.AST.Define (i, e1, e2) ->
    (* FIXME: The following code is wrong in general, hopefully, you
       FIXME: will implement [preprocess] in such a way that it will
       FIXME: work, right? *)
    let Source.AST.Id x = Position.value i in
    expression' (`Variable (Id x)) e1 @ expression' out e2

  | Source.AST.IfThenElse (c, t, f) ->
    let tc = expression' out t
    and fc = expression' out f in
    let l = fresh_label () in
    condition (first_label tc) (first_label fc) c
    @ tc
    @ [labelled (Jump l) ]
    @ fc
    @ [l, Comment "Join control point"]

  | Source.AST.FunCall (Source.AST.FunId "block_create", es) ->
    assign out BlockCreate es

  | Source.AST.FunCall (Source.AST.FunId "block_get", es) ->
    assign out BlockGet es

  | Source.AST.FunCall (Source.AST.FunId "block_set", es) ->
    assign out BlockSet es

  | Source.AST.FunCall (Source.AST.FunId f, es) when is_binop f ->
    assign out (binop f) es

  | Source.AST.FunCall (Source.AST.FunId f, actuals) ->
    as_rvalues actuals (fun xs ->
      labelled (Call (out,
                      `Immediate (LFun (FId f)),
                      xs))
    )

  | Source.AST.UnknownFunCall (ef, actuals) ->
    let f, ef = as_rvalue ef in
    ef @
      as_rvalues actuals (fun xs ->
        labelled (Call (out, f, xs))
      )
)
and as_rvalue e =
  let x = `Variable (fresh_variable ()) in
  (x, expression' x e)

and as_rvalues rs f =
  let xs, es = List.(split (map as_rvalue rs)) in
  List.flatten es @ [f xs]

and assign out op rs =
  as_rvalues rs (fun xs ->
    labelled (Target.AST.Assign (out, op, xs))
  )

and condition lt lf c = Target.AST.(
  let x = fresh_variable () in
  expression'(`Variable x) c
  @ [ labelled (ConditionalJump (EQ, [ `Variable x;
                                       `Immediate (LInt (Int32.of_int 0)) ],
                                 lf,
                                 lt))]
)

and expression' x e =
  let e, pos = Position.destruct e in
  expression pos x e



and first_label = function
  | [] -> assert false
  | (l, _) :: _ -> l

and labelled i =
  (fresh_label (), i)

and literal = Target.AST.(function
  | Source.AST.LInt x ->
    LInt (Int32.of_int x) (* FIXME *)
  | Source.AST.LFun (Source.AST.FunId f) ->
    LFun (FId f)
)

and is_binop = function
  | "+" | "-" | "*" | "/" -> true
  | c -> is_condition c

and is_condition = function
  | "<" | ">" | "=" | "<=" | ">=" -> true
  | _ -> false

and binop = Target.AST.(function
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | c -> Bool (condition_op c)
)

and condition_op = Target.AST.(function
  | "<" -> LT
  | ">" -> GT
  | "<=" -> LTE
  | ">=" -> GTE
  | "=" -> EQ
  | _ -> assert false
)

let preprocess p env =
  (p, env)


open Target.AST

module LabelMap = Map.Make (struct
    type t = Target.AST.label
    let compare = compare
end)


(** Control Flow Graph *)

(** A control flow graph is a directed graph whose edges
    are not labelled and whose nodes are labelled by
    labelled instructions. *)
type cfg = {
  label_to_node : labelled_instruction DiGraph.node LabelMap.t;
  graph : (unit, labelled_instruction) DiGraph.t
}

let string_of_labelled_instruction i =
  Str.(global_replace (regexp "\n") " " (
    RetrolixPrettyPrinter.(to_string (labelled_instruction 0) i)))

let control_flow_graph cfg (_, is) =
   failwith "Student, this is your job!"


type liveness_analysis_result = {
  live_in  : IdSet.t LabelMap.t;
  live_out : IdSet.t LabelMap.t;
}

let find_default d k m =
  try LabelMap.find k m with Not_found -> d

let empty_results =
  {
    live_in = LabelMap.empty;
    live_out = LabelMap.empty;
  }


let in_and_out cfg =
   failwith "Student, this is your job!"

let blocks =
  List.fold_left (fun bs -> function
    | DValue (_, b) | DFunction (_, _, b)  -> b :: bs
    | _ -> bs
  ) []

let liveness_analysis p =
  let cfg = { graph = DiGraph.empty; label_to_node = LabelMap.empty } in
  let cfg = List.fold_left control_flow_graph cfg (blocks p) in
  let live_in, live_out = in_and_out cfg.graph in
  cfg, { live_in; live_out }

let print_liveness_results r =
   failwith "Student, this is your job!"


(** [interference_graph globals liveness cfg] uses the static information
    about the [liveness] of variables in order to build a graph whose nodes
    are variables and edges are a conflict or a preference. *)
let interference_graph globals liveness cfg =
   failwith "Student, this is your job!"

type 'a coloring =
   unit


let colorize_graph (globals, onstack_arguments) graph all_colors =
   failwith "Student, this is your job!"

let allocable_registers =
MipsArch.(
  List.map (fun r -> `Register (RId (string_of_register r))) all_registers
)


let print_coloring coloring =
   failwith "Student, this is your job!"

let allocate_registers env coloring p =
   failwith "Student, this is your job!"


(** [optimize p cfg] translates [p] into a new equivalent program,
    more efficient, hopefully. *)
let optimize p cfg =
  failwith "Student! This is your job!"


(** [translate p env] turns the fopix program [p] into a semantically
    equivalent retrolix program. *)
let translate p env =
  failwith "Student! This is your job!"
