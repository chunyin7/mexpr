open Lex
open Ast

let parse toks = ()

let tok_to_binop = function
  | EQUAL -> Eq
  | LESS -> Lt
  | GREATER -> Gt
  | PLUS -> Add
  | MINUS -> Sub
  | TIMES -> Mul
  | DIVIDE -> Div
  | _ -> invalid_arg "Mismatched token type, expected binary operator."

let tok_to_unop = function
  | BANG -> BNeg
  | MINUS -> Neg
  | _ -> invalid_arg "Mismatched token type, expected unary operator."

let rec primary toks cur =
  let tok = List.nth toks cur in
  match tok with
  | BOOL b -> (Bool b, cur + 1)
  | INT i -> (Int i, cur + 1)
  | IDENT x -> (Var x, cur + 1)
  | LPAREN ->
      let e, cur' = expression toks (cur + 1) in
      if List.nth toks cur' = RPAREN then (e, cur' + 1)
      else failwith "Expected ')' after expression."
  | _ -> failwith "Unexpected token."

and unary toks cur =
  let tok = List.nth toks cur in
  match tok with
  | BANG | MINUS ->
      let right, cur' = unary toks (cur + 1) in
      (Unop (tok_to_unop tok, right), cur')
  | _ -> primary toks cur

and factor toks cur =
  let e, cur' = unary toks cur in

  let rec loop cur expr =
    if cur >= List.length toks then (expr, cur)
    else
      let tok = List.nth toks cur in
      match tok with
      | TIMES | DIVIDE ->
          let right, cur' = unary toks (cur + 1) in
          loop cur' (Binop (tok_to_binop tok, expr, right))
      | _ -> (expr, cur)
  in
  loop cur' e

and term toks cur =
  let e, cur' = factor toks cur in

  let rec loop cur expr =
    if cur >= List.length toks then (expr, cur)
    else
      let tok = List.nth toks cur in
      match tok with
      | MINUS | PLUS ->
          let right, cur' = factor toks (cur + 1) in
          loop cur' (Binop (tok_to_binop tok, expr, right))
      | _ -> (expr, cur)
  in
  loop cur' e

and comparison toks cur =
  let e, cur' = term toks cur in

  let rec loop cur expr =
    if cur >= List.length toks then (expr, cur)
    else
      let tok = List.nth toks cur in
      match tok with
      | GREATER | LESS ->
          let right, cur' = term toks (cur + 1) in
          loop cur' (Binop (tok_to_binop tok, expr, right))
      | _ -> (expr, cur)
  in
  loop cur' e

and equality toks cur =
  let e, cur' = comparison toks cur in

  let rec loop cur expr =
    if cur >= List.length toks then (expr, cur)
    else
      match List.nth toks cur with
      | EQUAL ->
          let right, cur' = comparison toks (cur + 1) in
          loop cur' (Binop (Eq, expr, right))
      | _ -> (expr, cur)
  in
  loop cur' e

and expression toks cur = equality toks cur
