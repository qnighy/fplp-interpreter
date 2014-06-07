(* eval_need.mli : Evaluation procedure, Call by Need *)
open Expr
open Thunk_need

exception Eval_error of expr * string

val lazy_eval : environment -> expr -> value
val eval : environment -> expr -> value_t

val eval_value_shallow : value -> value_t
val pp_eval_value : Format.formatter -> value -> unit
