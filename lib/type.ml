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
module IntSet = Set.Make (Int)

type tvar_set = IntSet.t

let empty_tvar_set = IntSet.empty

(* a free type var is one not bound for Forall *)

let rec ftv_ty = function
  | TVar var -> IntSet.singleton var
  | TFun (t1, t2) -> IntSet.union (ftv_ty t1) (ftv_ty t2)
  | _ -> empty_tvar_set

let ftv_scheme (Forall (tvars, ty)) =
  IntSet.diff (ftv_ty ty) (IntSet.of_list tvars)

let ftv_env env =
  StringMap.to_list env
  |> List.map (fun (_, ts) -> ts)
  |> List.fold_left
       (fun set ts -> IntSet.union set (ftv_scheme ts))
       empty_tvar_set

let generalise env t =
  let tvars = IntSet.diff (ftv_ty t) (ftv_env env) in
  Forall (IntSet.to_list tvars, t)

let rec apply_subs subs ty =
  match ty with
  | TVar var -> (
      match IntMap.find_opt var subs with Some sub -> sub | None -> ty)
  | TFun (t1, t2) ->
      let t1' = apply_subs subs t1 in
      let t2' = apply_subs subs t2 in
      TFun (t1', t2')
  | _ -> ty

let apply_subs_scheme subs (Forall (tvars, ty)) =
  let subs' = List.fold_right IntMap.remove tvars subs in
  let ty' = apply_subs subs' ty in
  Forall (tvars, ty')

let apply_subs_env subs env = StringMap.map (apply_subs_scheme subs) env

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

let rec unify subs t1 t2 =
  let t1 = apply_subs subs t1 in
  let t2 = apply_subs subs t2 in

  match (t1, t2) with
  | TInt, TInt | TBool, TBool | TUnit, TUnit -> subs
  | TVar a, TVar b when a = b -> subs
  | TVar a, t | t, TVar a -> bind_tvar subs a t
  | TFun (arg1, body1), TFun (arg2, body2) ->
      let subs' = unify subs arg1 arg2 in
      unify subs' body1 body2
  | _ -> failwith "Invalid unification."

(* env -> e -> ty * substitutions *)
let rec infer env e =
  match e with
  | Int _ -> (TInt, empty_sub)
  | Bool _ -> (TBool, empty_sub)
  | Unit -> (TUnit, empty_sub)
  | Var x ->
      let scheme = lookup env x in
      let ty = instantiate scheme in
      (ty, empty_sub)
  | Fun (arg, e) ->
      let a = fresh_tvar () in
      let env' = extend env arg (Forall ([], a)) in
      let body_ty, s = infer env' e in
      let t = apply_subs s (TFun (a, body_ty)) in
      (t, s)
  | Let (x, e1, e2) ->
      let t1, s1 = infer env e1 in
      let env' = apply_subs_env s1 env in
      let t1' = apply_subs s1 t1 in
      let scheme = generalise env' t1' in
      let env'' = extend env' x scheme in
      let t2, s2 = infer env'' e2 in
      let s = compose s2 s1 in
      (apply_subs s t2, s)
  | LetRec (x, e1, e2) ->
      let tx = fresh_tvar () in
      let recursive_env = extend env x (Forall ([], tx)) in
      let t1, s1 = infer recursive_env e1 in
      let s = unify s1 tx t1 in
      let t1' = apply_subs s t1 in
      let env' = apply_subs_env s env in
      let scheme = generalise env' t1' in
      let env'' = extend env' x scheme in
      let t2, s2 = infer env'' e2 in
      let s = compose s2 s in
      (apply_subs s t2, s)
  | Binop (bop, e1, e2) ->
      let l_ty, r_ty, ret_ty = lookup_bop bop in
      let t1, s1 = infer env e1 in
      let s1' = unify s1 t1 l_ty in
      let t2, s2 = infer (apply_subs_env s1' env) e2 in
      let s2' = unify (compose s2 s1') t2 r_ty in
      (apply_subs s2' ret_ty, s2')
  | Unop (op, e) ->
      let arg_ty, ret_ty = lookup_unop op in
      let ty, s = infer env e in
      let s' = unify s arg_ty ty in
      (apply_subs s' ret_ty, s')
  | If (e1, e2, e3) ->
      let t = fresh_tvar () in
      let t1, s1 = infer env e1 in
      let s1' = unify s1 t1 TBool in
      let env' = apply_subs_env s1' env in
      let t2, s2 = infer env' e2 in
      let s2' = unify (compose s2 s1') t2 t in
      let env'' = apply_subs_env s2' env in
      let t3, s3 = infer env'' e3 in
      let s3' = unify (compose s3 s2') t3 t in
      (apply_subs s3' t, s3')
  | Apply (e1, e2) ->
      let t = fresh_tvar () in
      let t1, s1 = infer env e1 in
      let env' = apply_subs_env s1 env in
      let t2, s2 = infer env' e2 in
      let s3 = unify (compose s2 s1) t1 (TFun (t2, t)) in
      (apply_subs s3 t, s3)

let infer_and_check e =
  let t, _s = infer StringMap.empty e in
  t
