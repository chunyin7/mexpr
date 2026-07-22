open Lex
open Ast

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

let rec primary toks =
  match toks with
  | BOOL b :: tl -> (Bool b, tl)
  | INT i :: tl -> (Int i, tl)
  | IDENT x :: tl -> (Var x, tl)
  | LPAREN :: tl -> (
      let e, tl' = expression tl in
      match tl' with
      | RPAREN :: tl'' -> (e, tl'')
      | _ -> failwith "Expected ')' after expression.")
  | _ -> failwith "Unexpected token."

and application toks =
  let f, toks' = primary toks in

  let rec loop fn toks =
    match toks with
    | (BOOL _ | INT _ | IDENT _ | LPAREN) :: _ ->
        let arg, toks' = primary toks in
        loop (Apply (fn, arg)) toks'
    | _ -> (fn, toks)
  in

  loop f toks'

and unary toks =
  match toks with
  | (BANG | MINUS) :: tl ->
      let hd = List.hd toks in
      let right, tl' = unary tl in
      (Unop (tok_to_unop hd, right), tl')
  | _ -> application toks

and factor toks =
  let e, toks' = unary toks in

  let rec loop toks expr =
    match toks with
    | (TIMES | DIVIDE) :: tl ->
        let hd = List.hd toks in
        let right, tl' = unary tl in
        loop tl' (Binop (tok_to_binop hd, expr, right))
    | _ -> (expr, toks)
  in
  loop toks' e

and term toks =
  let e, toks' = factor toks in

  let rec loop toks expr =
    match toks with
    | (MINUS | PLUS) :: tl ->
        let hd = List.hd toks in
        let right, tl' = factor tl in
        loop tl' (Binop (tok_to_binop hd, expr, right))
    | _ -> (expr, toks)
  in
  loop toks' e

and comparison toks =
  let e, toks' = term toks in

  let rec loop toks expr =
    match toks with
    | (GREATER | LESS) :: tl ->
        let hd = List.hd toks in
        let right, tl' = term tl in
        loop tl' (Binop (tok_to_binop hd, expr, right))
    | _ -> (expr, toks)
  in
  loop toks' e

and equality toks =
  let e, toks' = comparison toks in

  let rec loop toks expr =
    match toks with
    | EQUAL :: tl ->
        let right, tl' = comparison tl in
        loop tl' (Binop (Eq, expr, right))
    | _ -> (expr, toks)
  in
  loop toks' e

and conditional toks =
  match toks with
  | IF :: tl -> (
      let e1, tl = expression tl in
      match tl with
      | THEN :: tl -> (
          let e2, tl = expression tl in
          match tl with
          | ELSE :: tl ->
              let e3, tl = expression tl in
              (If (e1, e2, e3), tl)
          | _ -> (If (e1, e2, Unit), tl))
      | _ -> failwith "Expected 'then' after conditional.")
  | _ -> equality toks

and func toks =
  match toks with
  | FUN :: IDENT x :: ARROW :: tl ->
      let e, tl' = expression tl in
      (Fun (x, e), tl')
  | _ -> conditional toks

and bind toks =
  match toks with
  | LET :: IDENT x :: EQUAL :: tl -> (
      let expr, tl' = expression tl in
      match tl' with
      | IN :: tl ->
          let expr', tl' = bind tl in
          (Let (x, expr, expr'), tl')
      | _ -> failwith "Expected 'in' for let expr.")
  | _ -> func toks

and expression toks = bind toks

let parse toks =
  let rec loop toks asts =
    match toks with
    | hd :: _ when hd = EOF -> List.rev asts
    | [] -> List.rev asts
    | _ ->
        let ast, toks' = expression toks in
        loop toks' (ast :: asts)
  in
  loop toks []
