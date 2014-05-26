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
      "match", MATCH;
      "rec", REC;
      "then", THEN;
      "true", TRUE;
      "with", WITH;
    ];
    tbl
}

let digit = ['0'-'9']
let space = ' ' | '\t' | '\r'
let alpha = ['a'-'z' 'A'-'Z' '_']
let alnum = alpha | digit
let ident = alpha alnum*

rule token = parse
  | space+         { token lexbuf }
  | '\n'           { Lexing.new_line lexbuf; token lexbuf }
  | ident as ident {
      try
        Hashtbl.find kwd_tbl ident
      with Not_found ->
        IDENT ident
    }
  | ","            { COMMA }
  | "->"           { ARROW }
  | "::"           { DOUBLE_COLON }
  | ";;"           { DOUBLE_SEMICOLON }
  | ";"            { SEMICOLON }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '*'            { TIMES }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '/'            { DIVIDES }
  | '<'            { LT }
  | '='            { EQ }
  | '['            { LSQBRACKET }
  | ']'            { RSQBRACKET }
  | '|'            { VBAR }
  | digit+ as n    { NUM (int_of_string n) }
  | eof            { EOF }
  | _              { raise (LexingError (Loc.loc_from_lexer lexbuf))}

and skip_line = parse
  | '\n' { Lexing.new_line lexbuf; () }
  | _ { skip_line lexbuf }
