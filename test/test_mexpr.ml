open Mexpr
open Ast

let empty_env = Eval.STable.empty

let expect_eval name expected expression =
  let actual, _ = Eval.eval (expression, empty_env) in
  if actual <> expected then
    failwith
      (Printf.sprintf "%s: expression evaluated to an unexpected value" name)

let expect_token name expected lexeme =
  let actual = Lex.str_to_tok lexeme in
  if actual <> expected then
    failwith (Printf.sprintf "%s: lexeme produced an unexpected token" name)

let expect_invalid_token lexeme =
  match Lex.str_to_tok lexeme with
  | _ -> failwith (Printf.sprintf "%S should not be a valid token" lexeme)
  | exception Invalid_argument _ -> ()

let parse_one name source =
  match Parse.parse (Lex.lex source) with
  | [ expression ] -> expression
  | _ ->
      failwith
        (Printf.sprintf "%s: source did not parse to exactly one expression"
           name)

let expect_parse name expected source =
  let actual = parse_one name source in
  if actual <> expected then
    failwith (Printf.sprintf "%s: source produced an unexpected AST" name)

let expect_source_eval name expected source =
  expect_eval name expected (parse_one name source)

let () =
  List.iter
    (fun (lexeme, expected) -> expect_token lexeme expected lexeme)
    [
      ("", Lex.EOF);
      ("let", Lex.LET);
      ("in", Lex.IN);
      ("if", Lex.IF);
      ("then", Lex.THEN);
      ("else", Lex.ELSE);
      ("true", Lex.BOOL true);
      ("false", Lex.BOOL false);
      ("+", Lex.PLUS);
      ("-", Lex.MINUS);
      ("*", Lex.TIMES);
      ("/", Lex.DIVIDE);
      ("=", Lex.EQUAL);
      ("<", Lex.LESS);
      (">", Lex.GREATER);
      ("(", Lex.LPAREN);
      (")", Lex.RPAREN);
      ("123", Lex.INT 123);
      ("value_2", Lex.IDENT "value_2");
    ];
  expect_invalid_token "2value";
  expect_invalid_token "@";
  expect_eval "addition" (Int 7) (Binop (Add, Int 3, Int 4));
  expect_eval "nested arithmetic" (Int 14)
    (Binop (Mul, Binop (Add, Int 2, Int 5), Int 2));
  expect_eval "comparison" (Bool true) (Binop (Lt, Int 3, Int 8));
  expect_eval "conditional" (Int 10)
    (If (Binop (Eq, Int 4, Int 4), Int 10, Int 20));
  expect_eval "let binding" (Int 15)
    (Let ("x", Int 10, Binop (Add, Var "x", Int 5)));
  expect_eval "dependent let bindings" (Int 30)
    (Let
       ( "x",
         Int 10,
         Let ("y", Binop (Add, Var "x", Int 5), Binop (Mul, Var "y", Int 2)) ));
  expect_parse "operator precedence"
    (Binop (Add, Int 1, Binop (Mul, Int 2, Int 3)))
    "1 + 2 * 3";
  expect_parse "nested let initializer"
    (Let ("x", Let ("y", Int 2, Binop (Add, Var "y", Int 1)), Var "x"))
    "let x = let y = 2 in y + 1 in x";
  expect_parse "expression as conditional condition"
    (If (Let ("x", Int 2, Binop (Gt, Var "x", Int 1)), Int 3, Int 4))
    "if let x = 2 in x > 1 then 3 else 4";
  expect_parse "conditional without else"
    (If (Bool false, Int 1, Unit))
    "if false then 1";
  expect_source_eval "parsed conditional evaluation" (Int 10)
    "let x = 5 in if x > 3 then x * 2 else 0";
  expect_source_eval "parsed conditional without else evaluation" Unit
    "if false then 1"
