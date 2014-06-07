(* thunk.ml : evaluation status for cbn *)
open Misc
open Expr

type value =
  | VInt of int
  | VBool of bool
  | VTuple of value list
  | VNil
  | VCons of value * value
  | VFun of identifier * expr * environment
  | VThunk of expr * environment
and environment = {
  variables : value IdentifierMap.t
}

let rec pp_value pf = function
  | VInt i -> Format.fprintf pf "@[%d@]" i
  | VBool b -> Format.fprintf pf "@[%B@]" b
  | VTuple l ->
      Format.fprintf pf "@[(";
      let b = ref false in
      List.iter (fun x ->
        if !b then Format.fprintf pf ",@ ";
        b := true;
        pp_value pf x;
      ) l;
      Format.fprintf pf ")@]"
  | VNil ->
      Format.fprintf pf "@[[]@]"
  | VCons (h, t) ->
      Format.fprintf pf "@[[%a" pp_value h;
      pp_list_remaining pf t
  | VFun _ -> Format.fprintf pf "@[%s@]" "<fun>"
  | VThunk _ -> Format.fprintf pf "@[%s@]" "<thunk>"
and pp_list_remaining pf = function
  | VNil ->
      Format.fprintf pf "]@]"
  | VCons (h, t) ->
      Format.fprintf pf ";@ %a" pp_value h;
      pp_list_remaining pf t
  | x ->
      Format.fprintf pf "]@]@ @@@ %a" pp_value x

let empty_env = {
  variables = IdentifierMap.empty
}

let add_var v e env = {
  variables = IdentifierMap.add v e env.variables
}
