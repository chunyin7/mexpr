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

let apply_subs ty subs =
  match ty with
  | TVar var -> (
    match IntMap.find_opt var subs with
    | Some sub -> sub
    | None -> failwith "Applying substitution with no replacement."
  )
  | _ -> ty

let instantiate (Forall (vars, ty)) =
  let subs = List.fold_left (fun map var -> IntMap.add var (fresh_tvar ()) map) empty_sub vars in
  apply_subs ty subs

let occurs var ty = true

let bind_tvar subs var ty =
  let ty = apply_subs ty subs in

  if ty = TVar var then
    subs
  else if occurs var ty then
    failwith "Infinite type."
  else IntMap.add var ty subs

let rec unify subs t1 t2 =
  let t1 = apply_subs t1 subs in
  let t2 = apply_subs t2 subs in

  match t1, t2 with
  | TInt, TInt | TBool, TBool | TUnit, TUnit -> Ok subs
  | TVar a, TVar b when a = b -> Ok subs
  | TVar a, TInt | TInt, TVar a -> Ok (IntMap.add a TInt subs)
  | TVar a, TBool | TBool, TVar a -> IntMap.add a TBool empty_sub
  | TVar a, TUnit | TUnit, TVar a -> IntMap.add a TUnit empty_sub
  | TFun (arg1, body1), TFun (arg2, body2) ->
    let s1 = unify arg1 arg2 in

let rec infer env = function
  | Int _ -> empty_sub, TInt
  | Bool _ -> empty_sub, TBool
  | Unit -> empty_sub, TUnit
  | Var x ->
    let scheme = lookup env x in
    let ty = instantiate scheme in
    empty_sub, ty
  | Fun (arg, e) ->
    let a = fresh_tvar () in
    let env' = extend env arg (Forall ([], a)) in
    let subs, body_ty = infer env' e in
    let arg_ty = apply_subs a subs in
    subs, TFun (arg_ty, body_ty)
  | Let (x, e1, e2) | LetRec (x, e1, e2) ->
      let _, t1 = infer env e1 in
      let env' = extend env x (Forall ([], t1)) in
      infer env' e2
  | Binop (bop, e1, e2) -> (
      let _, t1 = infer env e1 in
      let _, t2 = infer env e2 in
      match (bop, t1, t2) with
      | Add, TInt, TInt | Mul, TInt, TInt | Div, TInt, TInt | Sub, TInt, TInt ->
          empty_sub, TInt
      | Eq, TInt, TInt | Lt, TInt, TInt | Gt, TInt, TInt -> empty_sub, TBool
      | _ -> failwith "Binary operator and operand type mismatch.")
  | Unop (op, e) -> (
      let subs, ty = infer env e in
      match (op, ty) with
      | BNeg, TBool -> empty_sub, TBool
      | Neg, TInt -> empty_sub, TInt
      | _ -> failwith "Unary operator and operand type mismatch.")
  | If (e1, e2, e3) ->
      if infer env e1 = TBool then
        let t2 = infer env e2 in
        if t2 = infer env e3 then t2
        else failwith "Branches of if must have same type."
      else failwith "Guard of if must have type bool."
  | Apply (f, arg) -> (
      let s1, f_ty = infer env f in
      let s2, arg_ty = infer env arg in
      let arg_ty' = apply_subs arg_ty s1 in
      match f_ty with
      | TFun (arg, body) when arg = arg_ty' -> s2, apply_subs body s2
      | TFun _ -> failwith "Mismatched argument types."
      | _ -> failwith "Applying non function type.")
