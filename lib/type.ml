type tvar = int
type typ = TUnit | TInt | TBool | TFun of typ * typ | TVar of tvar

(* represent polymorphic types *)
type tscheme = | Forall of tvar list * typ

module IntMap = Map.Make (Int)

(* id : type *)
type substitution = typ IntMap.t

(* t1 = t2 *)
type constraint_ = typ * typ

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

let rec occurs var ty =
  match ty with
  | TVar a -> a = var
  | TFun (t1, t2) -> (occurs var t1) || (occurs var t2)
  | _ -> false

let bind_tvar subs var ty =
  let ty = apply_subs ty subs in

  if ty = TVar var then
    subs
  else if occurs var ty then
    failwith "Infinite type."
  else IntMap.add var ty subs

(* let rec unify subs t1 t2 =
  let t1 = apply_subs t1 subs in
  let t2 = apply_subs t2 subs in

  match t1, t2 with
  | TInt, TInt | TBool, TBool | TUnit, TUnit -> Ok subs
  | TVar a, TVar b when a = b -> Ok subs
  | TVar a, TInt | TInt, TVar a -> Ok (IntMap.add a TInt subs)
  | TVar a, TBool | TBool, TVar a -> IntMap.add a TBool empty_sub
  | TVar a, TUnit | TUnit, TVar a -> IntMap.add a TUnit empty_sub
  | TFun (arg1, body1), TFun (arg2, body2) ->
    let s1 = unify arg1 arg2 in *)

let lookup_bop = function
  | Add | Sub | Div | Mul -> TInt, TInt, TInt
  | Eq | Lt | Gt -> TInt, TInt, TBool

let lookup_unop = function
  | BNeg -> TBool, TBool
  | Neg -> TInt, TInt

(* env -> e -> ty * constraints *)
let rec infer env e = match e with
  | Int _ -> TInt, []
  | Bool _ -> TBool, []
  | Unit -> TUnit, []
  | Var x ->
    let scheme = lookup env x in
    let ty = instantiate scheme in
    ty, []
  | Fun (arg, e) ->
    let a = fresh_tvar () in
    let env' = extend env arg (Forall ([], a)) in
    let body_ty, constraints = infer env' e in
    TFun (a, body_ty), constraints
  | Let (x, e1, e2) | LetRec (x, e1, e2) ->
      let t1, c1 = infer env e1 in
      let env' = extend env x (Forall ([], t1)) in
      let t2, c2 = infer env' e2 in
      t2, c1 @ c2
  | Binop (bop, e1, e2) ->
      let l_ty, r_ty, ret_ty = lookup_bop bop in
      let t1, c1 = infer env e1 in
      let t2, c2 = infer env e2 in
      let c = [(l_ty, t1); (r_ty, t1)] in
      let constraints = c @ c1 @ c2 in
      ret_ty, constraints
  | Unop (op, e) ->
      let arg_ty, ret_ty = lookup_unop op in
      let ty, c = infer env e in
      ret_ty, c @ [(arg_ty, ty)]
  | If (e1, e2, e3) ->
      let t = fresh_tvar () in
      let t1, c1 = infer env e1 in
      let t2, c2 = infer env e2 in
      let t3, c3 = infer env e3 in
      let c = [(t1, TBool); (t2, t); (t3, t)] in
      t, c @ c1 @ c2 @ c3
  | Apply (e1, e2) -> (
      let t = fresh_tvar () in
      let t1, c1 = infer env e1 in
      let t2, c2 = infer env e2 in
      let c = [(t1, TFun (t2, t))] in
      t, c @ c1 @ c2
