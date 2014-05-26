(* exprLexer.mll : lexer *)
{
  open ExprParser

  exception LexingError of string Loc.loc

  let kwd_tbl =
    let tbl = Hashtbl.create 3141 in
    List.iter (function (k,v) ->
      Hashtbl.add tbl k v
    ) [
      "and", ANDKWD;
      "else", ELSE;
      "false", FALSE;
      "fun", FUN;
      "if", IF;
      "in", IN;
      "let", LET;
      "rec", REC;
      "then", THEN;
      "true", TRUE;
    ];
    tbl
}

let digit = ['0'-'9']
let space = ' ' | '\t' | '\r' | '\n'
let alpha = ['a'-'z' 'A'-'Z' '_']
let alnum = alpha | digit
let ident = alpha alnum*

rule token = parse
  | space+         { token lexbuf }
  | ident as ident {
      try
        Hashtbl.find kwd_tbl ident
      with Not_found ->
        IDENT ident
    }
  | "->"           { ARROW }
  | ";;"           { DOUBLE_SEMICOLON }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '*'            { TIMES }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '/'            { DIVIDES }
  | '<'            { LT }
  | '='            { EQ }
  | digit+ as n    { NUM (int_of_string n) }
  | eof            { EOF }
  | _              { raise (LexingError (Loc.loc_from_lexer lexbuf))}

and skip_line = parse
  | '\n' { () }
  | _ { skip_line lexbuf }
