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

let unwrap_head = function
  | hd :: tl -> (hd, tl)
  | _ -> failwith "Unexpected eof."

let rec primary toks =
  let hd, tl = unwrap_head toks in
  match hd with
  | BOOL b -> (Bool b, tl)
  | INT i -> (Int i, tl)
  | IDENT x -> (Var x, tl)
  | LPAREN -> (
      let e, tl' = expression tl in
      match tl' with
      | hd' :: tl'' when hd' = RPAREN -> (e, tl'')
      | _ -> failwith "Expected ')' after expression.")
  | _ -> failwith "Unexpected token."

and unary toks =
  let hd, tl = unwrap_head toks in
  match hd with
  | BANG | MINUS ->
      let right, tl' = unary tl in
      (Unop (tok_to_unop hd, right), tl')
  | _ -> primary toks

and factor toks =
  let e, toks' = unary toks in

  let rec loop toks expr =
    match toks with
    | hd :: tl -> (
        match hd with
        | TIMES | DIVIDE ->
            let right, tl' = unary tl in
            loop tl' (Binop (tok_to_binop hd, expr, right))
        | _ -> (expr, toks))
    | _ -> (expr, toks)
  in
  loop toks' e

and term toks =
  let e, toks' = factor toks in

  let rec loop toks expr =
    match toks with
    | hd :: tl -> (
        match hd with
        | MINUS | PLUS ->
            let right, tl' = factor tl in
            loop tl' (Binop (tok_to_binop hd, expr, right))
        | _ -> (expr, toks))
    | _ -> (expr, toks)
  in
  loop toks' e

and comparison toks =
  let e, toks' = term toks in

  let rec loop toks expr =
    match toks with
    | hd :: tl -> (
        match hd with
        | GREATER | LESS ->
            let right, tl' = term toks in
            loop tl' (Binop (tok_to_binop hd, expr, right))
        | _ -> (expr, toks))
    | _ -> (expr, toks)
  in
  loop toks' e

and equality toks =
  let e, toks' = comparison toks in

  let rec loop toks expr =
    match toks with
    | hd :: tl -> (
        match hd with
        | EQUAL ->
            let right, tl' = comparison tl in
            loop tl' (Binop (tok_to_binop hd, expr, right))
        | _ -> (expr, toks))
    | _ -> (expr, toks)
  in
  loop toks' e

and expression toks = equality toks
