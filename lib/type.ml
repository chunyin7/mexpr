type tvar = int
type typ = TUnit | TInt | TBool | TFun of typ * typ | TVar of tvar

(* represent polymorphic types *)
type tscheme = Forall of tvar list * typ

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
  TVar id

open Ast

module StaticEnvironment = struct
  module StringMap = Map.Make (String)

  type t = tscheme StringMap.t

  let empty = StringMap.empty

  let lookup env x =
    match StringMap.find_opt x env with
    | Some ty -> ty
    | None -> failwith "Unbound variable."

  let extend env x ty = StringMap.add x ty env

  let lookup_bop = function
    | Add | Sub | Div | Mul -> (TInt, TInt, TInt)
    | Eq | Lt | Gt -> (TInt, TInt, TBool)

  let lookup_unop = function BNeg -> (TBool, TBool) | Neg -> (TInt, TInt)
end

open StaticEnvironment

let rec apply_subs subs ty =
  match ty with
  | TVar var -> (
      match IntMap.find_opt var subs with Some sub -> sub | None -> ty)
  | TFun (t1, t2) ->
      let t1' = apply_subs subs t1 in
      let t2' = apply_subs subs t2 in
      TFun (t1', t2')
  | _ -> ty

let instantiate (Forall (vars, ty)) =
  let subs =
    List.fold_left
      (fun map var -> IntMap.add var (fresh_tvar ()) map)
      empty_sub vars
  in
  apply_subs subs ty

let rec occurs var ty =
  match ty with
  | TVar a -> a = var
  | TFun (t1, t2) -> occurs var t1 || occurs var t2
  | _ -> false

let compose s2 s1 =
  (* apply s2 to types of s1 *)
  let s1' = IntMap.map (apply_subs s2) s1 in
  IntMap.union (fun _ _ t2 -> Some t2) s1' s2

let bind_tvar subs var ty =
  if ty = TVar var then subs
  else if occurs var ty then failwith "Infinite type."
  else compose (IntMap.singleton var ty) subs

let rec unify subs (t1, t2) =
  let t1 = apply_subs subs t1 in
  let t2 = apply_subs subs t2 in

  match (t1, t2) with
  | TInt, TInt | TBool, TBool | TUnit, TUnit -> subs
  | TVar a, TVar b when a = b -> subs
  | TVar a, t | t, TVar a -> bind_tvar subs a t
  | TFun (arg1, body1), TFun (arg2, body2) ->
      let subs' = unify subs (arg1, arg2) in
      unify subs' (body1, body2)
  | _ -> failwith "Invalid unification."

(* env -> e -> ty * constraints *)
let rec infer env e =
  match e with
  | Int _ -> (TInt, [])
  | Bool _ -> (TBool, [])
  | Unit -> (TUnit, [])
  | Var x ->
      let scheme = lookup env x in
      let ty = instantiate scheme in
      (ty, [])
  | Fun (arg, e) ->
      let a = fresh_tvar () in
      let env' = extend env arg (Forall ([], a)) in
      let body_ty, constraints = infer env' e in
      (TFun (a, body_ty), constraints)
  | Let (x, e1, e2) ->
      let t1, c1 = infer env e1 in
      let env' = extend env x (Forall ([], t1)) in
      let t2, c2 = infer env' e2 in
      (t2, c1 @ c2)
  | LetRec (x, e1, e2) ->
      let tx = fresh_tvar () in
      let env' = extend env x (Forall ([], tx)) in
      let t1, c1 = infer env' e1 in
      let t2, c2 = infer env' e2 in
      (t2, c1 @ c2 @ [ (tx, t1) ])
  | Binop (bop, e1, e2) ->
      let l_ty, r_ty, ret_ty = lookup_bop bop in
      let t1, c1 = infer env e1 in
      let t2, c2 = infer env e2 in
      let c = [ (l_ty, t1); (r_ty, t2) ] in
      let constraints = c @ c1 @ c2 in
      (ret_ty, constraints)
  | Unop (op, e) ->
      let arg_ty, ret_ty = lookup_unop op in
      let ty, c = infer env e in
      (ret_ty, c @ [ (arg_ty, ty) ])
  | If (e1, e2, e3) ->
      let t = fresh_tvar () in
      let t1, c1 = infer env e1 in
      let t2, c2 = infer env e2 in
      let t3, c3 = infer env e3 in
      let c = [ (t1, TBool); (t2, t); (t3, t) ] in
      (t, c @ c1 @ c2 @ c3)
  | Apply (e1, e2) ->
      let t = fresh_tvar () in
      let t1, c1 = infer env e1 in
      let t2, c2 = infer env e2 in
      let c = [ (t1, TFun (t2, t)) ] in
      (t, c @ c1 @ c2)

let infer_and_check e =
  let t, constraints = infer empty e in
  let subs = List.fold_left unify empty_sub constraints in
  apply_subs subs t
