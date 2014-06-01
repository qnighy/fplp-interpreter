(* type.mli : type *)
open Loc
open Expr
open Value

exception Type_error of unit loc * string

type existential = int

module ExistentialMap : Map.S with type key = existential

type ty =
  | TInt
  | TBool
  | TArrow of ty * ty
  | TTuple of ty list
  | TList of ty
  | TVar of int (* de brujin *)
  | TExistential of existential

type type_env = {
  existential_index: existential;
  existential_assign: ty ExistentialMap.t
}

type local_type_env = {
  variables: (int * ty) IdentifierMap.t
}
val empty_local_type_env : local_type_env

val add_type_var : identifier -> int -> ty -> local_type_env -> local_type_env

type typed_expr = (typed_expr_t * ty) loc
and typed_expr_t =
  | TEConstInt of int
  | TEConstBool of bool
  | TEVar of identifier
  | TEApp of typed_expr * typed_expr
  | TEFun of ty * identifier * typed_expr
  | TELet of int * ty * identifier * typed_expr * typed_expr
  | TERLet of int * (ty * identifier * typed_expr) list * typed_expr
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

val pp_ty_lvl : int -> Format.formatter -> ty -> unit
val pp_ty : Format.formatter -> ty -> unit
val pp_typed_expr_lvl : int -> Format.formatter -> typed_expr -> unit
val pp_typed_expr : Format.formatter -> typed_expr -> unit

val subst_type_ty : type_env -> ty -> ty
val subst_type_typed_expr :
  type_env -> typed_expr -> typed_expr

val infer_type_internal :
  type_env -> local_type_env ->
  expr -> type_env * typed_expr

val infer_type :
  local_type_env -> expr -> typed_expr

val infer_type_let :
  local_type_env -> identifier -> expr -> local_type_env * typed_expr

val infer_type_rlet :
  local_type_env ->
  (identifier * expr) list ->
  local_type_env * typed_expr list
