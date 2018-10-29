(** Register some compilers that have Retrolix as a target or source language. *)
let initialize () =
  Compilers.register "retrolix" "retrolix" (module Compilers.Identity (Retrolix));
  Compilers.register "fopix"   "retrolix" (module FopixToRetrolix)
