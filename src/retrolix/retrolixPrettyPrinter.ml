(** This module offers a pretty-printer for Stackix programs. *)

open PPrint
open PPrintCombinators
open PPrintEngine

open RetrolixAST

let located f x = f (Position.value x)

let max_label_length c =
  List.fold_left (fun m (Label l, _) -> max m (String.length l)) 0 c

let ( ++ ) x y =
  x ^^ break 1 ^^ y

let vcat = separate_map hardline (fun x -> x)

let rec program p =
  vcat (List.map definition p)

and definition = function
  | DValue (x, b) ->
    group (string "code" ++ parens (identifier x))
    ^^ hardline ^^ block b ++ string "end"
  | DFunction (f, xs, b) ->
    group (string "def"
           ++ function_identifier f
           ++ parens (identifiers xs))
    ++ string ":" ^^ hardline
    ^^ block b
  | DExternalFunction f ->
    group (string "external" ++ function_identifier f)

and block (ls, b) =
  let shift = max_label_length b in
  group (locals ls ^^ string ":") ++
  vcat (List.map (labelled_instruction shift) b)

and identifiers xs =
  separate_map (comma ^^ space) identifier xs

and identifier (Id x) =
  string x

and function_identifier (FId x) =
  string x

and locals = function
  | [] -> empty
  | xs -> group (string "local" ++ group (identifiers xs))

and labelled_instruction lsize (l, i) =
  group (label lsize l ^^ group (instruction i) ^^ string ";")

and label lsize (Label l) =
  string (Printf.sprintf "%*s: " lsize l)

and instruction = function
  | Call (l, f, xs) ->
    lvalue l ++ string "<-"
    ++ string "call" ++ rvalue f ++ parens (rvalues xs)

  | TailCall (f, xs) ->
    string "tail_call" ++ rvalue f ++ parens (rvalues xs)

  | Ret r ->
    string "ret" ++ rvalue r

  | Assign (l, o, rs) ->
    lvalue l ++ string "<-" ++ string (op o) ++ rvalues rs

  | Jump (Label l) ->
    string "jump" ++ string l

  | ConditionalJump (c, rs, Label l1, Label l2) ->
    string "jumpif" ++ string (condition c) ++ rvalues rs
    ++ string "->" ++ string l1 ^^ string ", " ++ string l2

  | Comment s ->
    string (";; " ^ s)

  | Exit ->
    string "exit"

and lvalue = function
  | `Variable x -> identifier x
  | `Register r -> register r

and rvalue = function
  | #lvalue as l -> lvalue l
  | `Immediate l -> literal l

and rvalues rs =
  separate_map (break 0 ^^ comma ^^ space) rvalue rs

and literal = function
  | LInt x -> string (Int32.to_string x)
  | LFun f -> string "&" ^^ function_identifier f

and register (RId x) = string x

and op = function
  | Load -> "load"
  | Add -> "add"
  | Mul -> "mul"
  | Div -> "div"
  | Sub -> "sub"
  | BlockCreate -> "block_create"
  | BlockSet -> "block_set"
  | BlockGet -> "block_get"
  | Bool c -> condition c

and condition = function
  | GT -> "gt"
  | LT -> "lt"
  | GTE -> "gte"
  | LTE -> "lte"
  | EQ -> "eq"

let instruction i = group (instruction i)

let to_string f x =
  let b = Buffer.create 13 in
  ToBuffer.pretty 0.5 80 b (f x);
  Buffer.contents b
