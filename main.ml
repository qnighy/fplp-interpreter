(* main.ml : Read-Eval-Print Loop *)
open Expr
open Value
open Type

let rec repl env lt_env lexbuf =
  try
    begin match ExprParser.command ExprLexer.token lexbuf with
    | CExp e ->
        Format.printf "@[%a@.@]" pp_typed_expr (Type.infer_type lt_env e);
        Format.printf "@[-@ =@ %a@.@]" pp_value (Eval.eval env e);
        repl env lt_env lexbuf
    | CLet (v, e) ->
        let ev = Eval.eval env e in
        Format.printf "@[%s@ =@ %a@.@]" v pp_value ev;
        repl (Value.add_var v ev env) lt_env lexbuf (* TODO *)
    | CRLet lal ->
        let lafuns = List.map (function
          | (x, { Loc.lval = EFun (y, e0) }) -> (x, y, e0)
          | (_, e) ->
              raise (Eval.Eval_error (e, "let-rec must be lambda expression."))
          ) lal in
        Format.printf "@[Defined.@.@]";
        repl (Value.add_rec_funs lafuns env) lt_env lexbuf (* TODO *)
    | CEnd -> ()
    end
  with
  | Eval.Eval_error (loc,msg) ->
      Format.printf "@[%a:@.Eval error:@ %s@.@]"
        Loc.loc_formatter loc
        msg;
      repl env lt_env lexbuf
  | ExprParser.Error ->
      Format.printf "@[%a:@.Parse error@.@]"
        Loc.loc_formatter (Loc.loc_from_lexer lexbuf);
      ExprLexer.skip_line lexbuf;
      repl env lt_env lexbuf
  | ExprLexer.LexingError loc ->
      Format.printf "@[%a:@.Lexing error:@ unknown token@.@]"
        Loc.loc_formatter loc;
      ExprLexer.skip_line lexbuf;
      repl env lt_env lexbuf

let () =
  let lexbuf = Lexing.from_channel
    (if Array.length Sys.argv >= 2 then
      open_in Sys.argv.(1)
    else
      stdin) in
  repl Value.empty_env Type.empty_local_type_env lexbuf
