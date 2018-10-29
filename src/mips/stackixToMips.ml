(** This module implements a compiler from Stackix to MIPS. *)

let error pos msg =
  Error.error "compilation" pos msg

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Stackix
module Target = Mips

(** We will need the following pieces of information to be carrying
    along the translation: *)
type environment = unit

(** Initially, the environment is empty. *)
let initial_environment () = ()

let make_label =
  let r = ref 0 in
  fun () ->
    incr r;
    Target.AST.Label ("l" ^ string_of_int !r)

(** [translate p env] turns a Stackix program [p] into a MIPS program
    using [env] to retrieve contextual information. *)
let v_stack_register = MipsArch.t 9
let v_stack_bottom_register = MipsArch.t 8
let exit_postlog_label = Target.AST.Label "exit_postlog"
let initialize_block_cells_label = Target.AST.Label "initial_block_cells_label"

let rec translate (p : Source.ast) env
: Target.ast * environment =
  failwith "Student, this is your job!"
and labelled f (l, i) =
  { Target.AST.label = Option.destruct make_label label l; value = f i }

and label = function
  | Source.AST.Label l -> Target.AST.Label l

and instruction p =
  failwith "Student, this is your job!"
