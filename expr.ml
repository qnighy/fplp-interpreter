(* expr.ml : value and expression *)
open Loc

type identifier = string

(* expression has its position data *)
type expr = expr_t loc
and expr_t =
  | EConstInt of int
  | EConstBool of bool
  | EVar of identifier
  | EApp of expr * expr
  | EFun of identifier * expr
  | ELet of identifier * expr * expr
  | ERLet of (identifier * expr) list * expr
  | EAdd of expr * expr
  | ESub of expr * expr
  | EMul of expr * expr
  | EDiv of expr * expr
  | EEq of expr * expr
  | ELt of expr * expr
  | EIf of expr * expr * expr

type command =
  | CExp of expr
  | CLet of identifier * expr
  | CRLet of (identifier * expr) list
  | CEnd
