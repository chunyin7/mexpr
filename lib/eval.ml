open Ast
module Env = Map.Make (String)

type value =
  | VUnit
  | VInt of int
  | VBool of bool
  | VClosure of string * expr * env

and thunk_state =
  | Suspended of expr * env
  | Forcing
  | Evaluated of value
  | Failed of exn

and thunk = thunk_state ref
and env = thunk Env.t

let eval_binop binop l r =
  match (binop, l, r) with
  | Add, VInt a, VInt b -> VInt (a + b)
  | Sub, VInt a, VInt b -> VInt (a - b)
  | Mul, VInt a, VInt b -> VInt (a * b)
  | Div, VInt a, VInt b -> VInt (a / b)
  | Eq, VInt a, VInt b -> VBool (a = b)
  | Lt, VInt a, VInt b -> VBool (a < b)
  | Gt, VInt a, VInt b -> VBool (a > b)
  | _ -> failwith "Binary operator and operand mismatch."

let eval_unop unop e =
  match (unop, e) with
  | Neg, VInt i -> VInt (-i)
  | BNeg, VBool b -> VBool (not b)
  | _ -> failwith "Unary operator and operand mismatch."

let rec eval expr env =
  match expr with
  | Int i -> VInt i
  | Bool b -> VBool b
  | Unit -> VUnit
  | Fun (arg, e) -> VClosure (arg, e, env)
  | Var x -> (
      match Env.find_opt x env with
      | Some thunk -> force thunk
      | None -> failwith ("Unbound variable: " ^ x))
  | Binop (op, left, right) ->
      let left = eval left env in
      let right = eval right env in
      eval_binop op left right
  | Unop (op, e) ->
      let e' = eval e env in
      eval_unop op e'
  | Let (x, e1, e2) ->
      let thunk = ref (Suspended (e1, env)) in
      let body_env = Env.add x thunk env in
      eval e2 body_env
  | If (e1, e2, e3) -> (
      match eval e1 env with
      | VBool true -> eval e2 env
      | VBool false -> eval e3 env
      | _ -> failwith "Non boolean condition.")
  | Apply (f, e) -> (
      let f = eval f env in
      match f with
      | VClosure (param, body, closure_env) ->
          let param_thunk = ref (Suspended (e, env)) in
          let call_env = Env.add param param_thunk closure_env in
          eval body call_env
      | _ -> failwith "Applying non-function expression.")

and force thunk =
  match !thunk with
  | Failed exn -> raise exn
  | Evaluated value -> value
  | Forcing -> failwith "Cyclic lazy binding."
  | Suspended (e, env) -> (
      thunk := Forcing;
      match eval e env with
      | value ->
          thunk := Evaluated value;
          value
      | exception exn ->
          thunk := Failed exn;
          raise exn)
