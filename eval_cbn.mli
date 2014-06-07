(* eval_cbn.mli : Evaluation procedure, Call by Name *)
open Expr
open Thunk

exception Eval_error of expr * string

val lazy_eval : environment -> expr -> value
val eval : environment -> expr -> value

val eval_value_shallow : value -> value
val eval_value_deep : value -> value
val pp_eval_value : Format.formatter -> value -> unit
