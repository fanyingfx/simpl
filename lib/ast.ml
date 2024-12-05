type bop =
  | Add
  | Mult
  | Leq

type typ =
  | TInt
  | TBool

type expr =
  | Var of string
  | Int of int
  | Binop of bop * expr * expr
  | Let of string * typ * expr * expr
  | Bool of bool
  | If of expr * expr * expr
