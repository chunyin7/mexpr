open Mexpr
open Ast

let infer_source name source =
  Type.infer_and_check (Test_support.parse_one name source)

let expect_type name expected source =
  Test_support.expect_equal name expected (infer_source name source)

let expect_type_failure name source =
  match infer_source name source with
  | _ -> failwith (Printf.sprintf "%s: expected type inference to fail" name)
  | exception Failure _ -> ()

let expect_identity_type name source =
  match infer_source name source with
  | Type.TFun (Type.TVar argument, Type.TVar result) when argument = result ->
      ()
  | _ -> failwith (Printf.sprintf "%s: expected an identity function type" name)

let expect_integer_application_type name source =
  match infer_source name source with
  | Type.TFun (Type.TFun (Type.TInt, Type.TVar result), Type.TVar returned)
    when result = returned ->
      ()
  | _ ->
      failwith
        (Printf.sprintf "%s: expected an (int -> 'a) -> 'a function type" name)

let () =
  expect_type "integer literal" Type.TInt "42";
  expect_type "boolean literal" Type.TBool "true";
  Test_support.expect_equal "unit literal" Type.TUnit
    (Type.infer_and_check Unit);
  expect_type "integer arithmetic" Type.TInt "1 + 2 * 3";
  expect_type "integer comparison" Type.TBool "1 < 2";
  expect_identity_type "identity function" "fun x -> x";
  expect_type "constrained function"
    (Type.TFun (Type.TInt, Type.TInt))
    "fun x -> x + 1";
  expect_type "function application" Type.TInt
    "let increment = fun x -> x + 1 in increment 4";
  expect_type "conditional branches" Type.TInt "if true then 1 else 2";
  expect_type "recursive function" Type.TInt
    "let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1) in fact 5";
  expect_type_failure "unbound variable" "missing";
  expect_type_failure "non-boolean condition" "if 0 then 1 else 2";
  expect_type_failure "different conditional branch types"
    "if true then 1 else false";
  expect_type_failure "non-function application" "1 2";
  expect_type_failure "invalid left operand" "false + 1";
  expect_type_failure "invalid right operand" "1 + false";
  expect_type_failure "infinite self-application" "fun x -> x x";
  expect_type "polymorphic let binding" Type.TBool
    "let id = fun x -> x in let number = id 1 in id true";
  expect_identity_type "polymorphic function with captured variable"
    "fun x -> let constant = fun y -> x in let number = constant 1 in constant \
     true";
  expect_integer_application_type "let binding retains inferred constraints"
    "fun f -> let result = f 1 in result";
  expect_type_failure "lambda argument remains monomorphic"
    "fun f -> let number = f 1 in f true";
  expect_type_failure "environment variable is not over-generalised"
    "fun f -> let result = f 1 in let number = result + 1 in if result then \
     true else false";
  expect_type "recursive binding is polymorphic after its definition" Type.TBool
    "let rec id = fun x -> x in let number = id 1 in id true";
  expect_type_failure "recursive binding remains monomorphic in its definition"
    "let rec f = fun x -> let number = f 1 in f true in f"
