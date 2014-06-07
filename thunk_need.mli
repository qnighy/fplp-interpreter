(* thunk_need.mli : evaluation status for Call by Need *)
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

val pp_value : Format.formatter -> value -> unit

val empty_env : environment
val add_var : identifier -> value -> environment -> environment
