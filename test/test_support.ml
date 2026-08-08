open Mexpr
open Ast

let parse_one name source =
  match Parse.parse (Lex.lex source) with
  | [ expression ] -> expression
  | _ ->
      failwith
        (Printf.sprintf "%s: source did not parse to exactly one expression"
           name)

let expect_equal name expected actual =
  if actual <> expected then
    failwith (Printf.sprintf "%s: unexpected result" name)
