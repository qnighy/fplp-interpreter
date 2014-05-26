(* loc.ml : location metadata for syntax tree *)
open Lexing

type 'a loc = {
  lval : 'a;
  lpos_start : position;
  lpos_end : position;
}

let loc_from_lexer lexbuf = {
  lval = Lexing.lexeme lexbuf;
  lpos_start = Lexing.lexeme_start_p lexbuf;
  lpos_end = Lexing.lexeme_end_p lexbuf
}

let loc_from_parser v = {
  lval = v;
  lpos_start = Parsing.symbol_start_pos ();
  lpos_end = Parsing.symbol_end_pos ()
}

let makeloc s e v = { lval = v; lpos_start = s; lpos_end = e }

let pos_formatter pf pos =
  if pos = dummy_pos then
    Format.fprintf pf "<Unknown position>"
  else
    Format.fprintf pf
      "File@ \"%s\",@ line@ %d,@ characters@ %d"
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)

let loc_formatter pf v =
  if v.lpos_start = dummy_pos then
    Format.fprintf pf "<Unknown position>"
  else if v.lpos_start.pos_fname != v.lpos_end.pos_fname then
    Format.fprintf pf
      ("File@ \"%s\",@ line@ %d,@ characters@ %d@ -@ " ^^
       "File@ \"%s\",@ line@ %d,@ characters@ %d")
      v.lpos_start.pos_fname
      v.lpos_start.pos_lnum
      (v.lpos_start.pos_cnum - v.lpos_start.pos_bol)
      v.lpos_end.pos_fname
      v.lpos_end.pos_lnum
      (v.lpos_end.pos_cnum - v.lpos_end.pos_bol)
  else if v.lpos_start.pos_lnum != v.lpos_end.pos_lnum then
    Format.fprintf pf
      ("File@ \"%s\",@ line@ %d,@ characters@ %d@ -@ " ^^
       "line@ %d,@ characters@ %d")
      v.lpos_start.pos_fname
      v.lpos_start.pos_lnum
      (v.lpos_start.pos_cnum - v.lpos_start.pos_bol)
      v.lpos_end.pos_lnum
      (v.lpos_end.pos_cnum - v.lpos_end.pos_bol)
  else
    Format.fprintf pf
      "File@ \"%s\",@ line@ %d,@ characters@ %d-%d"
      v.lpos_start.pos_fname
      v.lpos_start.pos_lnum
      (v.lpos_start.pos_cnum - v.lpos_start.pos_bol)
      (v.lpos_end.pos_cnum - v.lpos_end.pos_bol)
