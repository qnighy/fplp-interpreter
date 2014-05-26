/* exprParser.mly : parser */
%{
  open Loc
  open Expr
%}
%token<string> IDENT
%token<int> NUM
%token LPAREN RPAREN
%token PLUS MINUS TIMES DIVIDES EQ LT ARROW
%token IF THEN ELSE LET REC IN TRUE FALSE FUN ANDKWD
%token DOUBLE_SEMICOLON EOF

%nonassoc EQ LT
%left PLUS MINUS
%left TIMES DIVIDES

%start<Expr.command> command
%type<Expr.expr> expr expr2 expr3 expr4
%type<string Loc.loc> ident
%type<string * Expr.expr> let_statement
%type<(string * Expr.expr) list> let_rec_statement
%type<string * Expr.expr> let_rec_and_assignment
%type<string * Expr.expr> let_assignment
%%

command:
  | EOF                        { CEnd }
  | e = expr; DOUBLE_SEMICOLON { CExp e }
  | la = let_statement; DOUBLE_SEMICOLON { let (id, e) = la in CLet (id, e) }
  | lal = let_rec_statement; DOUBLE_SEMICOLON
      { CRLet lal }
;

expr:
  | la = let_statement; IN; e2 = expr;
      { let (id, e1) = la in makeloc $startpos $endpos (ELet (id, e1, e2)) }
  | lal = let_rec_statement; IN; e2 = expr;
      { makeloc $startpos $endpos (ERLet (lal, e2)) }
  | IF; cond = expr; THEN; e1 = expr; ELSE; e2 = expr
      { makeloc $startpos $endpos (EIf (cond, e1, e2)) }
  | FUN; binders = ident+; ARROW; e = expr
      {
        List.fold_right (fun x e ->
          makeloc x.lpos_start e.lpos_end (EFun (x.lval, e))
        ) binders e
      }
  | e = expr2             { e }
expr2:
  | e1 = expr2; PLUS; e2 = expr2
                          { makeloc $startpos $endpos (EAdd (e1, e2)) }
  | e1 = expr2; MINUS; e2 = expr2
                          { makeloc $startpos $endpos (ESub (e1, e2)) }
  | e1 = expr2; TIMES; e2 = expr2
                          { makeloc $startpos $endpos (EMul (e1, e2)) }
  | e1 = expr2; DIVIDES; e2 = expr2
                          { makeloc $startpos $endpos (EDiv (e1, e2)) }
  | e1 = expr2; EQ; e2 = expr2
                          { makeloc $startpos $endpos (EEq (e1, e2)) }
  | e1 = expr2; LT; e2 = expr2
                          { makeloc $startpos $endpos (ELt (e1, e2)) }
  | e = expr3             { e }

expr3:
  | el = expr4+ {
        List.fold_left (fun e1 e2 ->
          makeloc e1.lpos_start e2.lpos_end (EApp (e1, e2))
        ) (List.hd el) (List.tl el)
      }

expr4:
  | n = NUM               { makeloc $startpos $endpos (EConstInt n) }
  | TRUE                  { makeloc $startpos $endpos (EConstBool true) }
  | FALSE                 { makeloc $startpos $endpos (EConstBool false) }
  | id = IDENT            { makeloc $startpos $endpos (EVar id) }
  | LPAREN; e = expr; RPAREN { e }

ident:
  | x = IDENT { makeloc $startpos $endpos x }

let_statement:
  | LET; la = let_assignment { la }

let_rec_statement:
  | LET; REC; la = let_assignment; lal = let_rec_and_assignment*
      { la :: lal }

let_rec_and_assignment:
  | ANDKWD; la = let_assignment { la }

let_assignment:
  | id = IDENT; binders = ident*; EQ; e = expr
      {
        (id,
          List.fold_right
            (fun x y -> makeloc x.lpos_start y.lpos_end (EFun (x.lval, y)))
            binders e)
      }
