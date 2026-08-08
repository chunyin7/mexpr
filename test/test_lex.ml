open Mexpr

let expect_token lexeme expected =
  Test_support.expect_equal lexeme expected (Lex.str_to_tok lexeme)

let expect_invalid_token lexeme =
  match Lex.str_to_tok lexeme with
  | _ -> failwith (Printf.sprintf "%S should not be a valid token" lexeme)
  | exception Invalid_argument _ -> ()

let () =
  List.iter
    (fun (lexeme, expected) -> expect_token lexeme expected)
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
  expect_invalid_token "@"
