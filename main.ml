(* main.ml : Read-Eval-Print Loop *)
open Loc
open Expr
open Type

let rec repl env lt_env lexbuf =
  try
    begin match ExprParser.command ExprLexer.token lexbuf with
    | CExp e ->
        let et = Type.infer_type lt_env e in
        Format.printf "@[%a@.@]" pp_typed_expr et;
        Format.printf "@[:@ %a@.@]" pp_ty (snd et.lval);
        Format.printf "@[=@ %a@.@]" Value.pp_value (Eval.eval env e);
        repl env lt_env lexbuf
    | CLet (v, e) ->
        let (lt_env, et) = Type.infer_type_let lt_env v e in
        Format.printf "@[%s@ =@ %a@.@]" v pp_typed_expr et;
        Format.printf "@[:@ %a@.@]" pp_ty (snd et.lval);
        let ev = Eval.eval env e in
        Format.printf "@[=@ %a@.@]" Value.pp_value ev;
        repl (Value.add_var v ev env) lt_env lexbuf (* TODO *)
    | CRLet lal ->
        let (lt_env, ets) = Type.infer_type_rlet lt_env lal in
        let lafuns = List.map (function
          | (x, { lval = EFun (y, e0) }) -> (x, y, e0)
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
        loc_formatter loc
        msg;
      repl env lt_env lexbuf
  | Type.Type_error (loc,msg) ->
      Format.printf "@[%a:@.Type error:@ %s@.@]"
        loc_formatter loc
        msg;
      repl env lt_env lexbuf
  | ExprParser.Error ->
      Format.printf "@[%a:@.Parse error@.@]"
        loc_formatter (loc_from_lexer lexbuf);
      ExprLexer.skip_line lexbuf;
      repl env lt_env lexbuf
  | ExprLexer.LexingError loc ->
      Format.printf "@[%a:@.Lexing error:@ unknown token@.@]"
        loc_formatter loc;
      ExprLexer.skip_line lexbuf;
      repl env lt_env lexbuf

let rec repl_cbn env lt_env lexbuf =
  try
    begin match ExprParser.command ExprLexer.token lexbuf with
    | CExp e ->
        let et = Type.infer_type lt_env e in
        Format.printf "@[%a@.@]" pp_typed_expr et;
        Format.printf "@[:@ %a@.@]" pp_ty (snd et.lval);
        Format.printf "@[=@ %a@.@]"
          Eval_cbn.pp_eval_value (Eval_cbn.lazy_eval env e);
        repl_cbn env lt_env lexbuf
    | CLet (v, e) ->
        let (lt_env, et) = Type.infer_type_let lt_env v e in
        Format.printf "@[Defined.@.@]";
        (* Format.printf "@[%s@ =@ %a@.@]" v pp_typed_expr et;
        Format.printf "@[:@ %a@.@]" pp_ty (snd et.lval); *)
        let ev = Eval_cbn.lazy_eval env e in
        (* Format.printf "@[=@ %a@.@]" Eval_cbn.pp_eval_value ev; *)
        repl_cbn (Thunk.add_var v ev env) lt_env lexbuf (* TODO *)
    | CRLet lal ->
        let (lt_env, ets) = Type.infer_type_rlet lt_env lal in
        Format.printf "@[Defined.@.@]";
        let env =
          List.fold_left (fun fl_env -> function (x, e0) ->
            Thunk.add_var x
              (Thunk.VThunk (inherit_loc e0 (ERLet (lal, e0)), env))
              fl_env
          ) env lal
        in
        repl_cbn env lt_env lexbuf (* TODO *)
    | CEnd -> ()
    end
  with
  | Eval_cbn.Eval_error (loc,msg) ->
      Format.printf "@[%a:@.Eval error:@ %s@.@]"
        loc_formatter loc
        msg;
      repl_cbn env lt_env lexbuf
  | Type.Type_error (loc,msg) ->
      Format.printf "@[%a:@.Type error:@ %s@.@]"
        loc_formatter loc
        msg;
      repl_cbn env lt_env lexbuf
  | ExprParser.Error ->
      Format.printf "@[%a:@.Parse error@.@]"
        loc_formatter (loc_from_lexer lexbuf);
      ExprLexer.skip_line lexbuf;
      repl_cbn env lt_env lexbuf
  | ExprLexer.LexingError loc ->
      Format.printf "@[%a:@.Lexing error:@ unknown token@.@]"
        loc_formatter loc;
      ExprLexer.skip_line lexbuf;
      repl_cbn env lt_env lexbuf

let rec repl_need env lt_env lexbuf =
  try
    begin match ExprParser.command ExprLexer.token lexbuf with
    | CExp e ->
        let et = Type.infer_type lt_env e in
        Format.printf "@[%a@.@]" pp_typed_expr et;
        Format.printf "@[:@ %a@.@]" pp_ty (snd et.lval);
        Format.printf "@[=@ %a@.@]"
          Eval_need.pp_eval_value (Eval_need.lazy_eval env e);
        repl_need env lt_env lexbuf
    | CLet (v, e) ->
        let (lt_env, et) = Type.infer_type_let lt_env v e in
        Format.printf "@[Defined.@.@]";
        (* Format.printf "@[%s@ =@ %a@.@]" v pp_typed_expr et;
        Format.printf "@[:@ %a@.@]" pp_ty (snd et.lval); *)
        let ev = Eval_need.lazy_eval env e in
        (* Format.printf "@[=@ %a@.@]" Eval_need.pp_eval_value ev; *)
        repl_need (Thunk_need.add_var v ev env) lt_env lexbuf (* TODO *)
    | CRLet lal ->
        let (lt_env, ets) = Type.infer_type_rlet lt_env lal in
        Format.printf "@[Defined.@.@]";
        let env =
          List.fold_left (fun fl_env -> function (x, e0) ->
            Thunk_need.add_var x
              (ref (Thunk_need.VThunk
                      (inherit_loc e0 (ERLet (lal, e0)), env)))
              fl_env
          ) env lal
        in
        repl_need env lt_env lexbuf (* TODO *)
    | CEnd -> ()
    end
  with
  | Eval_need.Eval_error (loc,msg) ->
      Format.printf "@[%a:@.Eval error:@ %s@.@]"
        loc_formatter loc
        msg;
      repl_need env lt_env lexbuf
  | Type.Type_error (loc,msg) ->
      Format.printf "@[%a:@.Type error:@ %s@.@]"
        loc_formatter loc
        msg;
      repl_need env lt_env lexbuf
  | ExprParser.Error ->
      Format.printf "@[%a:@.Parse error@.@]"
        loc_formatter (loc_from_lexer lexbuf);
      ExprLexer.skip_line lexbuf;
      repl_need env lt_env lexbuf
  | ExprLexer.LexingError loc ->
      Format.printf "@[%a:@.Lexing error:@ unknown token@.@]"
        loc_formatter loc;
      ExprLexer.skip_line lexbuf;
      repl_need env lt_env lexbuf

let use_cbname = ref false
let use_cbneed = ref false
let use_channel = ref stdin

let arg_spec = [
  "--call-by-name", Arg.Set use_cbname, "Use call-by-name";
  "--call-by-need", Arg.Set use_cbneed, "Use call-by-need";
]

let () =
  Arg.parse arg_spec (fun x -> use_channel := open_in x)
    "./main [options] [<filename>]";
  let lexbuf = Lexing.from_channel !use_channel in
  if !use_cbname then
    repl_cbn Thunk.empty_env Type.empty_local_type_env lexbuf
  else if !use_cbneed then
    repl_need Thunk_need.empty_env Type.empty_local_type_env lexbuf
  else
    repl Value.empty_env Type.empty_local_type_env lexbuf
