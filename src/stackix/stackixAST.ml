(** The abstract syntax tree for stackix programs. *)

open Position

type t = labelled_instruction list

and labelled_instruction =
    label option * instruction located

and instruction =
  (** [Remember x] pousse l'entier [x] au sommet de la pile des
      résultats intermédiaires. *)
  | Remember of int
  (** [RememberLabel l] pousse une étiquette [l] au sommet de la pile
      des résultats intermédiaires. *)
  | RememberLabel of label
  (** [Swap] intervertit les deux cellules en haut de la pile des
      résultats intermédiaires. *)
  | Swap
  (** [Binop o] dépile [x1] et [x2] du sommet de la pile I et empile
      [x1 op x2] sur la pile I. *)
  | Binop of binop
  (** [Define x] dépile [v] du sommet de la pile I et empile [x = v]
      au sommet de la pile V. *)
  | Define of identifier
  (** [Undefine] dépile le sommet de la pile V. *)
  | Undefine
  (** [GetVariable i] met au sommet de la pile I la valeur de la variable
      à la profondeur [i] dans la pile V. *)
  | GetVariable of int
  (** [UJump] dépile l'étiquette [l] de la pile I et saute à cette étiquette. *)
  | UJump
  (** [Jump l] saute à l'étiquette [l]. *)
  | Jump of label
  (** [ConditionalJump [l1, l2]] dépile le booléen [b] et va en [l1] si [b = true]
      sinon en [l2]. *)
  | ConditionalJump of label * label
  (** [Exit] stoppe le programme. *)
  | Exit
  (** Laisse un commentaire dans le code. *)
  | Comment of string
  (** [BlockCreate] dépile [i] et [n] de la pile I et créé dans le tas un bloc de
      taille [n] dont les valeurs sont initialisées par la valeur [i]. *)
  | BlockCreate
  (** [BlockGet] dépile [idx] et [a] de la pile I et empile [*(a + idx)]
      au sommet de I. *)
  | BlockGet
  (** [BlockSet] dépile [v], [idx] et [a] de I, empile [unit] au
      sommet de I et écrit la valeur [v] dans le tas à la position [a
      + idx]. *)
  | BlockSet

and binop = Add | Mul | Div | Sub | GT | LT | GTE | LTE | EQ

and label = Label of string

and identifier = Id of string
