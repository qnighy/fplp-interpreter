(* eval.ml : Evaluation procedure *)
open Loc
open Expr
open Value

exception Eval_error of expr * string

let rec eval_pattern p v env =
  match p, v with
  | PConstInt i, VInt j when i = j -> Some env
  | PConstBool b, VBool d when b = d -> Some env
  | PVar x, _ -> Some (add_var x v env)
  | PTuple ps, VTuple l when List.length ps = List.length l ->
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

let rec eval env e =
  match e.lval with
  | EConstInt i -> VInt i
  | EConstBool b -> VBool b
  | EVar v ->
      begin try
        IdentifierMap.find v env.variables
      with Not_found ->
        raise (Eval_error (e, "Variable " ^ v ^ " not found"))
      end
  | EApp (e0, e1) ->
      begin match eval env e0 with
      | VFun (x, clos_e, clos_env) ->
          eval (add_var x (eval env e1) clos_env) clos_e
      | VRFun (lafuns, funname, clos_env) ->
          let (x, clos_e) = get_from_funname lafuns funname in
          eval (add_var x (eval env e1) (add_rec_funs lafuns clos_env)) clos_e
      | _ -> raise (Eval_error (e, "Illegal application: lhs is not a function."))
      end
  | EFun (x, e0) -> VFun (x, e0, env)
  | ELet (v, e0, e1) ->
      eval (add_var v (eval env e0) env) e1
  | ERLet (lal, e1) ->
      let lafuns = List.map (function
        | (x, { lval = EFun (y, e0) }) -> (x, y, e0)
        | _ -> raise (Eval_error (e, "let-rec must be lambda expression."))
        ) lal in
      eval (add_rec_funs lafuns env) e1
  | EMatch (e, ps) ->
      let v = eval env e in
      List.fold_right (function (p,pe) -> fun cont _ ->
        match eval_pattern p v env with
        | Some env -> eval env pe
        | None -> cont ()
      ) ps (fun _ -> raise (Eval_error (e, "Match Failure"))) ()
  | ETuple el -> VTuple (List.map (eval env) el)
  | ENil -> VNil
  | ECons (h, t) -> VCons (eval env h, eval env t)
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
