open Ast
module STable = Map.Make (String)

let is_val = function Int _ | Bool _ -> true | _ -> false
let unwrap_var expr = ()

let eval_binop binop e1 e2 =
  match (binop, e1, e2) with
  | Add, Int a, Int b -> Int (a + b)
  | Sub, Int a, Int b -> Int (a - b)
  | Mul, Int a, Int b -> Int (a * b)
  | Div, Int a, Int b -> Int (a / b)
  | Eq, Int a, Int b -> Bool (a = b)
  | Lt, Int a, Int b -> Bool (a < b)
  | Gt, Int a, Int b -> Bool (a > b)
  | _ -> failwith "Binary operator and operand mismatch."

let eval_unop unop e =
  match (unop, e) with
  | Neg, Int i -> Int (-i)
  | BNeg, Bool b -> Bool (not b)
  | _ -> failwith "Unary operator and operand mismatch."

let rec step (expr, s_table) =
  match expr with
  | Int _ | Bool _ -> failwith "Does not step eval"
  | Var x -> (
      match STable.find_opt x s_table with
      | Some e when is_val e -> (e, s_table)
      | Some e ->
          let e', s_table' = step (e, s_table) in
          (e', STable.add x e s_table')
      | None -> failwith "Unbound variable.")
  | Binop (op, e1, e2) when is_val e1 && is_val e2 ->
      (eval_binop op e1 e2, s_table)
  | Binop (op, e1, e2) when is_val e1 ->
      let e2', s_table' = step (e2, s_table) in
      (Binop (op, e1, e2'), s_table')
  | Binop (op, e1, e2) ->
      let e1', s_table' = step (e1, s_table) in
      (Binop (op, e1', e2), s_table')
  | Unop (op, e) when is_val e -> (eval_unop op e, s_table)
  | Unop (op, e) ->
      let e', s_table' = step (e, s_table) in
      (Unop (op, e'), s_table')
  | Let (x, e1, e2) -> (e2, STable.add x e1 s_table)
  | If (Bool true, e2, _) -> (e2, s_table)
  | If (Bool false, _, e3) -> (e3, s_table)
  | If (Int _, _, _) -> failwith "Non boolean condition."
  | If (e1, e2, e3) ->
      let e1', s_table' = step (e1, s_table) in
      (If (e1', e2, e3), s_table')

let rec eval (expr, s_table) =
  if is_val expr then (expr, s_table) else (expr, s_table) |> step |> eval
