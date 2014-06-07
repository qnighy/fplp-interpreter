(* eval_need.ml : Evaluation procedure, Call by Need *)
open Misc
open Loc
open Expr
open Thunk_need

exception Eval_error of expr * string

let lazy_eval env e = ref (VThunk (e, env))

let rec eval_pattern p v env =
  match p, !v with
  | PVar x, _ -> Some (add_var x v env)
  | _, VThunk (ve, venv) ->
      eval_pattern_val p (eval_value_shallow v) env
  | _, VValue vv ->
      eval_pattern_val p vv env
and eval_pattern_val p vv env =
  match p, vv with
  | PConstInt i, VInt j when i = j -> Some env
  | PConstBool b, VBool d when b = d -> Some env
  | PTuple ps, VTuple l
        when List.length ps = List.length l ->
      List.fold_right (function (p,v) -> fun cont env ->
        match eval_pattern p v env with
        | Some env -> cont env
        | None -> None
      ) (List.combine ps l) (fun env -> Some env) env
  | PNil, VNil -> Some env
  | PCons (p0,p1), VCons (v0,v1) ->
      begin match eval_pattern p0 v0 env with
      | Some env -> eval_pattern p1 v1 env
      | None -> None
      end
  | _ -> None
and eval env e =
  match e.lval with
  | EConstInt i -> VInt i
  | EConstBool b -> VBool b
  | EVar v ->
      begin try
        eval_value_shallow (IdentifierMap.find v env.variables)
      with Not_found ->
        raise (Eval_error (e, "Variable " ^ v ^ " not found"))
      end
  | EApp (e0, e1) ->
      begin match eval env e0 with
      | VFun (x, clos_e, clos_env) ->
          eval (add_var x (lazy_eval env e1) clos_env) clos_e
      | _ -> raise (Eval_error (e, "Illegal application: lhs is not a function."))
      end
  | EFun (x, e0) -> VFun (x, e0, env)
  | ELet (v, e0, e1) ->
      eval (add_var v (lazy_eval env e0) env) e1
  | ERLet (lal, e1) ->
      let env0 =
        List.fold_left (fun fl_env -> function (x, e0) ->
          add_var x (ref (VThunk (inherit_loc e0 (ERLet (lal, e0)), env)))
                  fl_env
        ) env lal
      in
      eval env0 e1
  | EMatch (e, ps) ->
      let v = lazy_eval env e in
      List.fold_right (function (p,pe) -> fun cont _ ->
        match eval_pattern p v env with
        | Some env -> eval env pe
        | None -> cont ()
      ) ps (fun _ -> raise (Eval_error (e, "Match Failure"))) ()
  | ETuple el -> VTuple (List.map (lazy_eval env) el)
  | ENil -> VNil
  | ECons (h, t) -> VCons (lazy_eval env h, lazy_eval env t)
  | EAdd (e0, e1) ->
      begin match eval env e0, eval env e1 with
      | VInt i0, VInt i1 -> VInt (i0 + i1)
      | _, VInt _ -> raise (Eval_error (e0, "Can't cast bool to int"))
      | _, _ -> raise (Eval_error (e1, "Can't cast bool to int"))
      end
  | ESub (e0, e1) ->
      begin match eval env e0, eval env e1 with
      | VInt i0, VInt i1 -> VInt (i0 - i1)
      | _, VInt _ -> raise (Eval_error (e0, "Can't cast bool to int"))
      | _, _ -> raise (Eval_error (e1, "Can't cast bool to int"))
      end
  | EMul (e0, e1) ->
      begin match eval env e0, eval env e1 with
      | VInt i0, VInt i1 -> VInt (i0 * i1)
      | _, VInt _ -> raise (Eval_error (e0, "Can't cast bool to int"))
      | _, _ -> raise (Eval_error (e1, "Can't cast bool to int"))
      end
  | EDiv (e0, e1) ->
      begin match eval env e0, eval env e1 with
      | VInt i0, VInt i1 -> VInt (i0 / i1)
      | _, VInt _ -> raise (Eval_error (e0, "Can't cast bool to int"))
      | _, _ -> raise (Eval_error (e1, "Can't cast bool to int"))
      end
  | EEq (e0, e1) ->
      begin match eval env e0, eval env e1 with
      | VInt i0, VInt i1 -> VBool (i0 = i1)
      | VBool b0, VBool b1 -> VBool (b0 = b1)
      | _, _ -> raise (Eval_error (e, "Can't compare between bool and int"))
      end
  | ELt (e0, e1) ->
      begin match eval env e0, eval env e1 with
      | VInt i0, VInt i1 -> VBool (i0 < i1)
      | _, VInt _ -> raise (Eval_error (e0, "Can't cast bool to int"))
      | _, _ -> raise (Eval_error (e1, "Can't cast bool to int"))
      end
  | EIf (e0, e1, e2) ->
      begin match eval env e0 with
      | VBool true -> eval env e1
      | VBool false -> eval env e2
      | _ -> raise (Eval_error (e0, "Can't cast int to bool"))
      end
and eval_value_shallow v =
  match !v with
  | VThunk (e, env) ->
      let vv = eval env e in
      v := VValue vv;
      vv
  | VValue vv -> vv

let rec pp_eval_value pf v =
  match eval_value_shallow v with
  | VInt i -> Format.fprintf pf "@[%d@]" i
  | VBool b -> Format.fprintf pf "@[%B@]" b
  | VTuple l ->
      Format.fprintf pf "@[(";
      let b = ref false in
      List.iter (fun x ->
        if !b then Format.fprintf pf ",@ ";
        b := true;
        pp_eval_value pf x;
      ) l;
      Format.fprintf pf ")@]"
  | VNil ->
      Format.fprintf pf "@[[]@]"
  | VCons (h, t) ->
      Format.fprintf pf "@[[%a" pp_eval_value h;
      pp_eval_list_remaining pf t
  | VFun _ -> Format.fprintf pf "@[%s@]" "<fun>"
and pp_eval_list_remaining pf v =
  match eval_value_shallow v with
  | VNil ->
      Format.fprintf pf "]@]"
  | VCons (h, t) ->
      Format.fprintf pf ";@ %a" pp_eval_value h;
      pp_eval_list_remaining pf t
  | _ ->
      Format.fprintf pf "]@]@ @@@ %a" pp_eval_value v
