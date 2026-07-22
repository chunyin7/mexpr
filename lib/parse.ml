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
  let hd, tl = unwrap_head toks in
  match hd with
  | BANG | MINUS ->
      let right, tl' = unary tl in
      (Unop (tok_to_unop hd, right), tl')
  | _ -> application toks

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
            let right, tl' = term tl in
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

and conditional toks =
  match toks with
  | hd :: tl when hd = IF -> (
      let e1, tl = expression tl in
      match tl with
      | hd :: tl when hd = THEN -> (
          let e2, tl = expression tl in
          match tl with
          | hd :: tl when hd = ELSE ->
              let e3, tl = expression tl in
              (If (e1, e2, e3), tl)
          | _ -> (If (e1, e2, Unit), tl))
      | _ -> failwith "Expected 'then' after conditional.")
  | _ -> equality toks

and func toks =
  match toks with
  | FUN :: IDENT x :: ARROW :: d ->
      let e, tl = expression d in
      (Fun (x, e), tl)
  | _ -> conditional toks

and bind toks =
  match toks with
  | LET :: IDENT x :: EQUAL :: d -> (
      let expr, d' = expression d in
      match d' with
      | hd :: tl when hd = IN ->
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
