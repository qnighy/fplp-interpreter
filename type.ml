(* type.ml : type *)
open Loc
open Expr
open Value

exception Unification_error
exception Type_error of unit loc * string

let type_error l msg =
  Type_error (inherit_loc l (), msg)

type existential = int

module Existential = struct
  type t = existential
  let compare x y = if x < y then -1 else if x = y then 0 else 1
end

module ExistentialMap = Map.Make(Existential)

type ty =
  | TInt
  | TBool
  | TArrow of ty * ty
  | TTuple of ty list
  | TList of ty
  | TVar of int (* de brujin *)
  | TForall of ty
  | TExistential of existential

type type_env = {
  existential_index: existential;
  existential_assign: ty ExistentialMap.t
}

type local_type_env = {
  variables: ty IdentifierMap.t
}

let empty_type_env = {
  existential_index = 0;
  existential_assign = ExistentialMap.empty
}

let empty_local_type_env = {
  variables = IdentifierMap.empty
}

let add_type_var v t env = {
  variables = IdentifierMap.add v t env.variables
}

type typed_expr = (typed_expr_t * ty) loc
and typed_expr_t =
  | TEConstInt of int
  | TEConstBool of bool
  | TEVar of identifier
  | TEApp of typed_expr * typed_expr
  | TEFun of ty * identifier * typed_expr
  | TELet of ty * identifier * typed_expr * typed_expr
  | TERLet of (ty * identifier * typed_expr) list * typed_expr
  | TEMatch of typed_expr * (pattern * typed_expr) list
  | TETuple of typed_expr list
  | TENil
  | TECons of typed_expr * typed_expr
  | TEAdd of typed_expr * typed_expr
  | TESub of typed_expr * typed_expr
  | TEMul of typed_expr * typed_expr
  | TEDiv of typed_expr * typed_expr
  | TEEq of typed_expr * typed_expr
  | TELt of typed_expr * typed_expr
  | TEIf of typed_expr * typed_expr * typed_expr

let pp_ty_getlevel = function
  | TInt -> 0
  | TBool -> 0
  | TArrow _ -> 2
  | TTuple _ -> 0
  | TList _ -> 1
  | TVar _ -> 0
  | TForall _ -> 3
  | TExistential _ -> 0

let rec pp_ty_lvl lvl pf t =
  if pp_ty_getlevel t > lvl then
    Format.fprintf pf "@[("
  else ();
  begin match t with
  | TInt -> Format.fprintf pf "@[int@]"
  | TBool -> Format.fprintf pf "@[bool@]"
  | TArrow (t0, t1) ->
      Format.fprintf pf "@[%a@ ->@ %a@]"
        (pp_ty_lvl 1) t0
        (pp_ty_lvl 2) t1
  | TTuple tl ->
      Format.fprintf pf "@[(";
      let b = ref true in
      List.iter (fun t ->
        begin if !b then
          Format.fprintf pf "%a" (pp_ty_lvl 10) t
        else
          Format.fprintf pf ",@ %a" (pp_ty_lvl 10) t
        end;
        b := false) tl;
      Format.fprintf pf ")@]"
  | TList t -> Format.fprintf pf "@[%a@ list@]" (pp_ty_lvl 1) t
  | TVar i -> Format.fprintf pf "@[VAR%d@]" i
  | TForall t -> Format.fprintf pf "@[FORALL@ %a@]" (pp_ty_lvl 3) t
  | TExistential ex -> Format.fprintf pf "@[?%d@]" ex
  end;
  if pp_ty_getlevel t > lvl then
    Format.fprintf pf ")@]"
  else ()

let pp_ty = pp_ty_lvl 10

let rec is_list_literal e =
  match fst e.lval with
  | TENil -> true
  | TECons (_, e1) -> is_list_literal e1
  | _ -> false

let pp_typed_expr_getlevel e =
  match fst e.lval with
  | TEConstInt _ -> 0
  | TEConstBool _ -> 0
  | TEVar _ -> 0
  | TEApp _ -> 1
  | TEFun _ -> 6
  | TELet _ -> 6
  | TERLet _ -> 6
  | TEMatch _ -> 6
  | TETuple _ -> 6
  | TENil -> 0
  | TECons _ -> if is_list_literal e then 0 else 4
  | TEAdd _ -> 3
  | TESub _ -> 3
  | TEMul _ -> 2
  | TEDiv _ -> 2
  | TEEq _ -> 5
  | TELt _ -> 5
  | TEIf _ -> 6

let rec pp_typed_expr_lvl lvl pf e =
  if pp_typed_expr_getlevel e > lvl then
    Format.fprintf pf "@[("
  else ();
  begin match fst e.lval with
  | TEConstInt i -> Format.fprintf pf "@[%d@]" i
  | TEConstBool b -> Format.fprintf pf "@[%B@]" b
  | TEVar x -> Format.fprintf pf "@[%s@]" x
  | TEApp (e0, e1) ->
      Format.fprintf pf "@[%a@ %a@]"
        (pp_typed_expr_lvl 1) e0
        (pp_typed_expr_lvl 0) e1
  | TEFun (t_abs, x_abs, e) ->
      Format.fprintf pf "@[fun@ %s@ :@ %a@ ->@ %a@]"
        x_abs
        pp_ty t_abs
        (pp_typed_expr_lvl 10) e
  | TELet (t_let, x_let, e0, e1) ->
      Format.fprintf pf "@[(let@ %s@ :@ %a@ :=@ %a@ in@ %a)@]"
        x_let
        pp_ty t_let
        (pp_typed_expr_lvl 10) e0
        (pp_typed_expr_lvl 10) e1
  | TERLet (lafuns, e1) ->
      Format.fprintf pf "@[";
      let flag = ref true in
      List.iter (function (t_rlet, x_rlet, e0) ->
        if !flag then
          Format.fprintf pf "@[let@ rec@ "
        else
          Format.fprintf pf "@[and@ ";
        Format.fprintf pf "%s@ :@ %a@ :=@ %a@)@]@ "
          x_rlet
          pp_ty t_rlet
          (pp_typed_expr_lvl 10) e0
      ) lafuns;
      Format.fprintf pf "in@ %a)@]"
        (pp_typed_expr_lvl 10) e1
  | TEMatch (e, ps) ->
      Format.fprintf pf "@[match@ %a@ with@ "
        (pp_typed_expr_lvl 10) e;
      List.iter (function (p,pe) ->
        Format.fprintf pf "@[|@ %a@ ->@ %a@]"
          pp_pattern p
          (pp_typed_expr_lvl 10) pe
      ) ps;
      Format.fprintf pf "@]"
  | TETuple el ->
      Format.fprintf pf "@[(";
      let b = ref true in
      List.iter (fun t ->
        begin if !b then
          Format.fprintf pf "%a" (pp_typed_expr_lvl 10) t
        else
          Format.fprintf pf ",@ %a" (pp_typed_expr_lvl 10) t
        end;
        b := false) el;
      Format.fprintf pf ")@]"
  | TENil -> Format.fprintf pf "@[[]@]"
  | TECons (e0, e1) ->
      if is_list_literal e then
        Format.fprintf pf "@[[%a%a]@]"
          (pp_typed_expr_lvl 10) e0
          pp_typed_expr_list e1
      else
        Format.fprintf pf "@[%a@ ::@ %a@]"
          (pp_typed_expr_lvl 3) e0
          (pp_typed_expr_lvl 4) e1
  | TEAdd (e0, e1) ->
      Format.fprintf pf "@[%a@ +@ %a@]"
        (pp_typed_expr_lvl 3) e0
        (pp_typed_expr_lvl 2) e1
  | TESub (e0, e1) ->
      Format.fprintf pf "@[%a@ -@ %a@]"
        (pp_typed_expr_lvl 3) e0
        (pp_typed_expr_lvl 2) e1
  | TEMul (e0, e1) ->
      Format.fprintf pf "@[%a@ *@ %a@]"
        (pp_typed_expr_lvl 2) e0
        (pp_typed_expr_lvl 1) e1
  | TEDiv (e0, e1) ->
      Format.fprintf pf "@[%a@ /@ %a@]"
        (pp_typed_expr_lvl 2) e0
        (pp_typed_expr_lvl 1) e1
  | TEEq (e0, e1) ->
      Format.fprintf pf "@[%a@ =@ %a@]"
        (pp_typed_expr_lvl 4) e0
        (pp_typed_expr_lvl 4) e1
  | TELt (e0, e1) ->
      Format.fprintf pf "@[%a@ <@ %a@]"
        (pp_typed_expr_lvl 4) e0
        (pp_typed_expr_lvl 4) e1
  | TEIf (e0, e1, e2) ->
      Format.fprintf pf "@[if@ %a@ then@ %a@ else@ %a@]"
        (pp_typed_expr_lvl 6) e0
        (pp_typed_expr_lvl 6) e1
        (pp_typed_expr_lvl 6) e2
  end;
  if pp_typed_expr_getlevel e > lvl then
    Format.fprintf pf ")@]"
  else ()
and pp_typed_expr_list pf e =
  match fst e.lval with
  | TENil -> ()
  | TECons (e0, e1) ->
      Format.fprintf pf ";@ %a%a"
        (pp_typed_expr_lvl 10) e0
        pp_typed_expr_list e1
  | _ -> assert false

let pp_typed_expr = pp_typed_expr_lvl 10

let new_existential env =
  ({ env with existential_index = env.existential_index + 1 },
   TExistential env.existential_index)

let rec subst_type_ty env t =
  match t with
  | TInt | TBool -> t
  | TArrow (t0, t1) ->
      TArrow (subst_type_ty env t0,
              subst_type_ty env t1)
  | TTuple tl -> TTuple (List.map (subst_type_ty env) tl)
  | TList t -> TList (subst_type_ty env t)
  | TVar _ -> t
  | TForall t -> TForall (subst_type_ty env t)
  | TExistential ex ->
      begin try
        subst_type_ty env
          (ExistentialMap.find ex env.existential_assign)
      (* with Not_found -> failwith "atode_subst_type_ty" *) (* TODO *)
      with Not_found -> t
      end

let rec subst_type_typed_expr env te =
  let old_expr = fst te.lval in
  let new_expr = match old_expr with
  | TEConstInt i -> old_expr
  | TEConstBool b -> old_expr
  | TEVar x -> old_expr
  | TEApp (e0, e1) ->
      TEApp (subst_type_typed_expr env e0,
             subst_type_typed_expr env e1)
  | TEFun (t_abs, x_abs, e) ->
      TEFun (subst_type_ty env t_abs,
             x_abs, subst_type_typed_expr env e)
  | TELet (t_let, x_let, e0, e1) ->
      TELet (subst_type_ty env t_let,
             x_let,
             subst_type_typed_expr env e0,
             subst_type_typed_expr env e1)
  | TERLet (lafuns, e1) ->
      TERLet (
        List.map (function (t_rlet, x_rlet, e0) ->
          (subst_type_ty env t_rlet,
           x_rlet,
           subst_type_typed_expr env e0)) lafuns,
        subst_type_typed_expr env e1)
  | TEMatch (e0, ps) ->
      TEMatch (
        subst_type_typed_expr env e0,
        List.map (function (p,pe) ->
          (p, subst_type_typed_expr env pe)) ps)
  | TETuple el ->
      TETuple (List.map (subst_type_typed_expr env) el)
  | TENil -> old_expr
  | TECons (e0, e1) ->
      TECons (subst_type_typed_expr env e0,
              subst_type_typed_expr env e1)
  | TEAdd (e0, e1) ->
      TEAdd (subst_type_typed_expr env e0,
             subst_type_typed_expr env e1)
  | TESub (e0, e1) ->
      TESub (subst_type_typed_expr env e0,
             subst_type_typed_expr env e1)
  | TEMul (e0, e1) ->
      TEMul (subst_type_typed_expr env e0,
             subst_type_typed_expr env e1)
  | TEDiv (e0, e1) ->
      TEDiv (subst_type_typed_expr env e0,
             subst_type_typed_expr env e1)
  | TEEq (e0, e1) ->
      TEEq (subst_type_typed_expr env e0,
            subst_type_typed_expr env e1)
  | TELt (e0, e1) ->
      TELt (subst_type_typed_expr env e0,
            subst_type_typed_expr env e1)
  | TEIf (e0, e1, e2) ->
      TEIf (subst_type_typed_expr env e0,
            subst_type_typed_expr env e1,
            subst_type_typed_expr env e2)
  in
  inherit_loc te (new_expr, subst_type_ty env (snd te.lval))

let rec existential_occurence ex t =
  match t with
  | TInt | TBool -> false
  | TArrow (t0, t1) ->
      existential_occurence ex t0 ||
      existential_occurence ex t1
  | TTuple tl ->
      List.for_all (existential_occurence ex) tl
  | TList t -> existential_occurence ex t
  | TVar _ -> false
  | TForall t -> existential_occurence ex t
  | TExistential ex0 -> ex = ex0

let rec unify_type env t0 t1 =
  (* Format.eprintf "@[unify:@ %a@ =@ %a@.@]"
    pp_ty t0 pp_ty t1; *)
  match t0, t1 with
  | TExistential ex0, _ ->
      begin try
        let t0 = ExistentialMap.find ex0 env.existential_assign in
        unify_type env t0 t1
      with Not_found ->
        if existential_occurence ex0 t1 then
          raise Unification_error
        else
          { env with
            existential_assign =
              ExistentialMap.add ex0 t1 env.existential_assign
          }
      end
  | _, TExistential ex1 ->
      begin try
        let t1 = ExistentialMap.find ex1 env.existential_assign in
        unify_type env t0 t1
      with Not_found ->
        if existential_occurence ex1 t0 then
          raise Unification_error
        else
          { env with
            existential_assign =
              ExistentialMap.add ex1 t0 env.existential_assign
          }
      end
  | TInt, TInt -> env
  | TBool, TBool -> env
  | TArrow (t00, t01), TArrow (t10, t11) ->
      let env = unify_type env t00 t10 in
      unify_type env t01 t11
  | TTuple tl0, TTuple tl1 when List.length tl0 = List.length tl1 ->
      List.fold_left (fun env -> function (t0, t1) ->
        unify_type env t0 t1
      ) env (List.combine tl0 tl1)
  | TList t0, TList t1 -> unify_type env t0 t1
  | _, _ -> raise Unification_error

let rec unify_type_of_term env trm typ =
  try
    unify_type env (snd trm.lval) typ
  with
  | Unification_error ->
      let trm = subst_type_typed_expr env trm in
      let typ = subst_type_ty env typ in
      raise (type_error trm
        (Format.asprintf
          ("@[The@ expression@ @[%a@]@ has@ type@ %a@.@]" ^^
           "@[But it was expected of type %a@]")
          pp_typed_expr trm
          pp_ty (snd trm.lval)
          pp_ty typ))

let rec infer_type_internal env l_env e =
  match e.lval with
  | EConstInt i -> (env, inherit_loc e (TEConstInt i, TInt))
  | EConstBool b -> (env, inherit_loc e (TEConstBool b, TBool))
  | EVar v ->
      begin try
        let t = IdentifierMap.find v l_env.variables in
        (env, inherit_loc e (TEVar v, t))
      with Not_found ->
        raise (type_error e ("Variable " ^ v ^ " not found"))
      end
  | EApp (e0, e1) ->
      let (env,ex0) = new_existential env in
      let (env,ex1) = new_existential env in
      let (env,e0t) = infer_type_internal env l_env e0 in
      let (env,e1t) = infer_type_internal env l_env e1 in
      let env = unify_type_of_term env e0t (TArrow (ex0, ex1)) in
      let env = unify_type_of_term env e1t ex0 in
      (env, inherit_loc e (TEApp (e0t, e1t), ex1))
  | EFun (x, e0) ->
      let (env,ex0) = new_existential env in
      let l_env0 = add_type_var x ex0 l_env in
      let (env,e0t) = infer_type_internal env l_env0 e0 in
      (env, inherit_loc e (TEFun (ex0, x, e0t), TArrow (ex0, snd e0t.lval)))
  (* | ELet of identifier * expr * expr
  | ERLet of (identifier * expr) list * expr
  | EMatch of expr * (pattern * expr) list *)
  | ETuple el ->
      let (env, rev_elt) =
        List.fold_left (function (env,rev_elt) -> fun e0 ->
          let (env,e0t) = infer_type_internal env l_env e0 in
          (env, e0t :: rev_elt)
        ) (env,[]) el in
      let elt = List.rev rev_elt in
      (env, inherit_loc e
        (TETuple elt,
         TTuple (List.map (fun e0t -> snd e0t.lval) elt)))
  | ENil ->
      let (env,ex0) = new_existential env in
      (env, inherit_loc e (TENil, TList ex0))
  | ECons (e0, e1) ->
      let (env,e0t) = infer_type_internal env l_env e0 in
      let (env,e1t) = infer_type_internal env l_env e1 in
      let env = unify_type_of_term env e1t (TList (snd e0t.lval)) in
      (env, inherit_loc e (TECons (e0t, e1t), snd e1t.lval))
  | EAdd (e0, e1) ->
      let (env,e0t) = infer_type_internal env l_env e0 in
      let (env,e1t) = infer_type_internal env l_env e1 in
      let env = unify_type_of_term env e0t TInt in
      let env = unify_type_of_term env e1t TInt in
      (env, inherit_loc e (TEAdd (e0t, e1t), TInt))
  | ESub (e0, e1) ->
      let (env,e0t) = infer_type_internal env l_env e0 in
      let (env,e1t) = infer_type_internal env l_env e1 in
      let env = unify_type_of_term env e0t TInt in
      let env = unify_type_of_term env e1t TInt in
      (env, inherit_loc e (TESub (e0t, e1t), TInt))
  | EMul (e0, e1) ->
      let (env,e0t) = infer_type_internal env l_env e0 in
      let (env,e1t) = infer_type_internal env l_env e1 in
      let env = unify_type_of_term env e0t TInt in
      let env = unify_type_of_term env e1t TInt in
      (env, inherit_loc e (TEMul (e0t, e1t), TInt))
  | EDiv (e0, e1) ->
      let (env,e0t) = infer_type_internal env l_env e0 in
      let (env,e1t) = infer_type_internal env l_env e1 in
      let env = unify_type_of_term env e0t TInt in
      let env = unify_type_of_term env e1t TInt in
      (env, inherit_loc e (TEDiv (e0t, e1t), TInt))
  | EEq (e0, e1) ->
      let (env,e0t) = infer_type_internal env l_env e0 in
      let (env,e1t) = infer_type_internal env l_env e1 in
      let env = unify_type_of_term env e1t (snd e0t.lval) in
      (env, inherit_loc e (TELt (e0t, e1t), TBool))
  | ELt (e0, e1) ->
      let (env,e0t) = infer_type_internal env l_env e0 in
      let (env,e1t) = infer_type_internal env l_env e1 in
      let env = unify_type_of_term env e0t TInt in
      let env = unify_type_of_term env e1t TInt in
      (env, inherit_loc e (TELt (e0t, e1t), TBool))
  | EIf (e0, e1, e2) ->
      let (env,e0t) = infer_type_internal env l_env e0 in
      let (env,e1t) = infer_type_internal env l_env e1 in
      let (env,e2t) = infer_type_internal env l_env e2 in
      let env = unify_type_of_term env e0t TBool in
      let env = unify_type_of_term env e2t (snd e1t.lval) in
      (env, inherit_loc e (TEIf (e0t, e1t, e2t), (snd e1t.lval)))
  (* | _ -> failwith "unsupported expr for type inference" *)

let infer_type l_env e =
  let env = empty_type_env in
  let (env, te) = infer_type_internal env l_env e in
  let te = subst_type_typed_expr env te in
  te
