type binop = Add | Sub | Mul | Div | Eq | Lt | Gt
type unop = BNeg | Neg

type expr =
  | Unit
  | Int of int
  | Bool of bool
  | Var of string
  | Binop of binop * expr * expr
  | Unop of unop * expr
  | Let of string * expr * expr (* let name = expr in expr *)
  | If of expr * expr * expr (* if expr then expr else expr *)
  | Fun of string * expr (* fun x -> expr *)
  | Apply of expr * expr (* fun - val for param *)
