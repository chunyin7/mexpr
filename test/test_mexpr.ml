open Mexpr
open Ast

let empty_env = Eval.STable.empty

let expect_eval name expected expression =
  let actual, _ = Eval.eval (expression, empty_env) in
  if actual <> expected then
    failwith (Printf.sprintf "%s: expression evaluated to an unexpected value" name)

let () =
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