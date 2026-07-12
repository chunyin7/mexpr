open Mexpr
open Ast

let empty_env = Eval.STable.empty

let expect_eval name expected expression =
  let actual, _ = Eval.eval (expression, empty_env) in
  if actual <> expected then
    failwith (Printf.sprintf "%s: expression evaluated to an unexpected value" name)

let expect_token name expected lexeme =
  let actual = Lex.str_to_tok lexeme in
  if actual <> expected then
    failwith (Printf.sprintf "%s: lexeme produced an unexpected token" name)

let expect_invalid_token lexeme =
  match Lex.str_to_tok lexeme with
  | _ -> failwith (Printf.sprintf "%S should not be a valid token" lexeme)
  | exception Invalid_argument _ -> ()

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
         Let
           ( "y",
             Binop (Add, Var "x", Int 5),
             Binop (Mul, Var "y", Int 2) ) ))