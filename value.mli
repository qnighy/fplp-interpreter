(* value.mli : value and evaluation environment *)
open Expr

module IdentifierMap : Map.S with type key = identifier

type value =
  | VInt of int
  | VBool of bool
  | VFun of identifier * expr * environment
  | VRFun of (identifier * identifier * expr) list * identifier * environment
and environment = {
  variables : value IdentifierMap.t
}

val value_formatter : Format.formatter -> value -> unit

val empty_env : environment
val add_var : identifier -> value -> environment -> environment
val add_rec_funs : (identifier * identifier * expr) list -> environment -> environment
val get_from_funname :
  (identifier * identifier * expr) list -> identifier -> identifier * expr
