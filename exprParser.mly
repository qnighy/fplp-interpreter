/* exprParser.mly : parser */
%{
  open Loc
  open Expr
%}
%token<string> IDENT
%token<int> NUM
%token LPAREN RPAREN
%token PLUS MINUS TIMES DIVIDES EQ LT ARROW VBAR
%token IF THEN ELSE LET REC IN TRUE FALSE FUN ANDKWD MATCH WITH
%token DOUBLE_SEMICOLON EOF

%nonassoc EQ LT
%left PLUS MINUS
%left TIMES DIVIDES

%start<Expr.command> command
%type<Expr.expr> expr expr3 expr4 expr5
%type<string Loc.loc> ident
%type<string * Expr.expr> let_statement
%type<(string * Expr.expr) list> let_rec_statement
%type<string * Expr.expr> let_rec_and_assignment
%type<string * Expr.expr> let_assignment

%type<Expr.pattern> pattern
%type<Expr.pattern> pattern_clause_tail
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
  | MATCH; e = expr; WITH; ps = pattern_arrows
      {
        makeloc $startpos $endpos (EMatch (e, ps))
      }
  | e = expr3             { e }
expr3:
  | e1 = expr3; PLUS; e2 = expr3
                          { makeloc $startpos $endpos (EAdd (e1, e2)) }
  | e1 = expr3; MINUS; e2 = expr3
                          { makeloc $startpos $endpos (ESub (e1, e2)) }
  | e1 = expr3; TIMES; e2 = expr3
                          { makeloc $startpos $endpos (EMul (e1, e2)) }
  | e1 = expr3; DIVIDES; e2 = expr3
                          { makeloc $startpos $endpos (EDiv (e1, e2)) }
  | e1 = expr3; EQ; e2 = expr3
                          { makeloc $startpos $endpos (EEq (e1, e2)) }
  | e1 = expr3; LT; e2 = expr3
                          { makeloc $startpos $endpos (ELt (e1, e2)) }
  | e = expr4             { e }

expr4:
  | el = expr5+ {
        List.fold_left (fun e1 e2 ->
          makeloc e1.lpos_start e2.lpos_end (EApp (e1, e2))
        ) (List.hd el) (List.tl el)
      }

expr5:
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

pattern:
  | n = NUM    { PConstInt n }
  | TRUE       { PConstBool true }
  | FALSE      { PConstBool false }
  | id = IDENT { PVar id }
  | LPAREN; p = pattern; RPAREN { p }

pattern_arrows:
  | p = pattern_arrow_head; pt = pattern_arrow_tail* { p @ (List.concat pt) }

pattern_arrow_head:
  | VBAR?; p = pattern; ps = pattern_clause_tail*; ARROW; e = expr
      { List.map (fun p -> (p, e)) (p::ps) }

pattern_arrow_tail:
  | ps = pattern_clause_tail+; ARROW; e = expr { List.map (fun p -> (p, e)) ps }

pattern_clause_tail:
  | VBAR; p = pattern { p }
