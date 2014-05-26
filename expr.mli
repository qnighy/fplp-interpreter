(* expr.mli : value and expression *)
open Loc

type identifier = string

type pattern =
  | PConstInt of int
  | PConstBool of bool
  | PVar of identifier
  | PTuple of pattern list
  | PNil
  | PCons of pattern * pattern

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
  | EMatch of expr * (pattern * expr) list
  | ETuple of expr list
  | ENil
  | ECons of expr * expr
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
