type tvar = int
type typ = TUnit | TInt | TBool | TFun of typ * typ | TVar of tvar

(* represent polymorphic types *)
type tscheme = | Forall of tvar list * typ

module IntMap = Map.Make (Int)

(* id : type *)
type substitution = typ IntMap.t

let empty_sub = IntMap.empty

(* id generator for tvar *)
let next_id = ref 0

let fresh_tvar () =
  let id = !next_id in
  incr next_id;
  TVar (id)

module StringMap = Map.Make (String)

module StaticEnvironment = struct
  type t = tscheme StringMap.t

  let empty = StringMap.empty

  let lookup env x =
    match StringMap.find_opt x env with
    | Some ty -> ty
    | None -> failwith "Unbound variable."

  let extend env x ty = StringMap.add x ty env
end

open StaticEnvironment
open Ast

let apply_sub ty subs =
  match ty with
  | TVar var -> (
    match IntMap.find_opt var subs with
    | Some sub -> sub
    | None -> failwith "Applying substitution with no replacement."
  )
  | _ -> ty

let instantiate (Forall (vars, ty)) =
  let subs = List.fold_left (fun map var -> IntMap.add var (fresh_tvar ()) map) empty_sub vars in
  apply_sub ty subs

let rec infer env = function
  | Int _ -> empty_sub, TInt
  | Bool _ -> empty_sub, TBool
  | Unit -> empty_sub, TUnit
  | Var x ->
    let scheme = lookup env x in
    let ty = instantiate scheme in
    empty_sub, ty
  | Fun (arg, e) ->
    let arg_ty = fresh_tvar () in
    let env' = extend env arg arg_ty in
    let subs, body_ty = infer env' e in
    subs, TFun (arg_ty, body_ty)
  | Let (x, e1, e2) | LetRec (x, e1, e2) ->
      let _, t1 = infer env e1 in
      let env' = extend env x t1 in
      infer env' e2
  | Binop (bop, e1, e2) -> (
      let t1 = infer env e1 in
      let t2 = infer env e2 in
      match (bop, t1, t2) with
      | Add, TInt, TInt | Mul, TInt, TInt | Div, TInt, TInt | Sub, TInt, TInt ->
          TInt
      | Eq, TInt, TInt | Lt, TInt, TInt | Gt, TInt, TInt -> TBool
      | _ -> failwith "Binary operator and operand type mismatch.")
  | Unop (op, e) -> (
      let ty = infer env e in
      match (op, ty) with
      | BNeg, TBool -> TBool
      | Neg, TInt -> TInt
      | _ -> failwith "Unary operator and operand type mismatch.")
  | If (e1, e2, e3) ->
      if infer env e1 = TBool then
        let t2 = infer env e2 in
        if t2 = infer env e3 then t2
        else failwith "Branches of if must have same type."
      else failwith "Guard of if must have type bool."
  | Apply (e1, e2) -> (
      let t1 = infer env e1 in
      let t2 = infer env e2 in
      match t1 with
      | TFun (arg, body) when arg = t2 -> body
      | _ -> failwith "Applying non function type.")

and unify = function
  | TUnit, TUnit | TInt, TInt | TBool, TBool -> ()
  | TVar a, TInt
