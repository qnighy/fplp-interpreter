(* thunk_need.ml : evaluation status for Call by Need *)
open Misc
open Expr

type value = value_sel ref
and value_sel =
  | VValue of value_t
  | VThunk of expr * environment
and value_t =
  | VInt of int
  | VBool of bool
  | VTuple of value list
  | VNil
  | VCons of value * value
  | VFun of identifier * expr * environment
and environment = {
  variables : value IdentifierMap.t
}

let rec pp_value pf v =
  match !v with
  | VValue vv -> begin match vv with
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
    end
  | VThunk _ -> Format.fprintf pf "@[%s@]" "<thunk>"
and pp_list_remaining pf v =
  match !v with
  | VValue vv -> begin match vv with
    | VNil ->
        Format.fprintf pf "]@]"
    | VCons (h, t) ->
        Format.fprintf pf ";@ %a" pp_value h;
        pp_list_remaining pf t
    | _ ->
        Format.fprintf pf "]@]@ @@@ %a" pp_value v
    end
  | VThunk _ -> Format.fprintf pf "]@]@ @@@ %a" pp_value v

let empty_env = {
  variables = IdentifierMap.empty
}

let add_var v e env = {
  variables = IdentifierMap.add v e env.variables
}
