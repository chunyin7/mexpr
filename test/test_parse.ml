open Mexpr
open Ast

let expect_parse name expected source =
  Test_support.expect_equal name expected (Test_support.parse_one name source)

let () =
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
    "if false then 1"
