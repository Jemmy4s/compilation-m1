(** Register some compilers that have MIPS as a target or source language. *)
let initialize () =
  Compilers.register "stackix"    "mips" (module StackixToMips);
  Compilers.register "retrolix"   "mips" (module RetrolixToMips)
