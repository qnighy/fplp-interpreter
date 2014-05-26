(* value.ml : value and evaluation environment *)
open Expr

module IdentifierMap = Map.Make(String)

type value =
  | VInt of int
  | VBool of bool
  | VFun of identifier * expr * environment
  | VRFun of (identifier * identifier * expr) list * identifier * environment
and environment = {
  variables : value IdentifierMap.t
}

let value_formatter pf = function
  | VInt i -> Format.fprintf pf "%d" i
  | VBool b -> Format.fprintf pf "%B" b
  | VFun _ -> Format.fprintf pf "<fun>"
  | VRFun _ -> Format.fprintf pf "<rec-fun>"

let empty_env = {
  variables = IdentifierMap.empty
}

let add_var v e env = {
  variables = IdentifierMap.add v e env.variables
}

let add_rec_funs lafuns env =
  List.fold_left (fun fl_env -> function (x, _, _) ->
    add_var x (VRFun (lafuns, x, env)) fl_env) env lafuns

let rec get_from_funname lafuns funname =
  match lafuns with
  | [] -> raise Not_found
  | (x, y, e) :: lafuns_tail ->
      if x = funname then (y, e) else get_from_funname lafuns_tail funname
