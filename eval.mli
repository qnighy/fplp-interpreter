(* eval.mli : Evaluation procedure *)
open Expr
open Value

exception Eval_error of expr * string

val eval : environment -> expr -> value
