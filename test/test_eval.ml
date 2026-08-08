open Mexpr
open Ast

let empty_env = Eval.Env.empty

let expect_eval name expected expression =
  Test_support.expect_equal name expected (Eval.eval expression empty_env)

let expect_source_eval name expected source =
  expect_eval name expected (Test_support.parse_one name source)

let () =
  expect_eval "addition" (Eval.VInt 7) (Binop (Add, Int 3, Int 4));
  expect_eval "nested arithmetic" (Eval.VInt 14)
    (Binop (Mul, Binop (Add, Int 2, Int 5), Int 2));
  expect_eval "comparison" (Eval.VBool true) (Binop (Lt, Int 3, Int 8));
  expect_eval "conditional" (Eval.VInt 10)
    (If (Binop (Eq, Int 4, Int 4), Int 10, Int 20));
  expect_eval "let binding" (Eval.VInt 15)
    (Let ("x", Int 10, Binop (Add, Var "x", Int 5)));
  expect_eval "dependent let bindings" (Eval.VInt 30)
    (Let
       ( "x",
         Int 10,
         Let ("y", Binop (Add, Var "x", Int 5), Binop (Mul, Var "y", Int 2)) ));
  expect_source_eval "parsed conditional evaluation" (Eval.VInt 10)
    "let x = 5 in if x > 3 then x * 2 else 0";
  expect_source_eval "parsed conditional without else evaluation" Eval.VUnit
    "if false then 1";
  expect_source_eval "curried function application" (Eval.VInt 30)
    "let f = fun x -> fun y -> x * y in f 5 6";
  expect_source_eval "recursive factorial" (Eval.VInt 120)
    "let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1) in fact 5";
  expect_source_eval "recursive fibonacci" (Eval.VInt 55)
    "let rec fib = fun n -> if n < 2 then n else fib (n - 1) + fib (n - 2) in \
     fib 10";
  expect_source_eval "unused recursive binding" (Eval.VInt 42)
    "let rec x = x in 42"
