type binop = Add | Sub | Mul | Div | Eq | Lt 

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Binop of binop * expr * expr
  | Let of string * expr * expr (* let name = expr in expr *)
  | If of expr * expr * expr (* if expr then expr else expr *)
