(* thunk.mli : evaluation status for cbn *)
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

val pp_value : Format.formatter -> value -> unit

val empty_env : environment
val add_var : identifier -> value -> environment -> environment
