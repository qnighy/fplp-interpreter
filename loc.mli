(* loc.mli : location metadata for syntax tree *)
open Lexing

type 'a loc = {
  lval : 'a;
  lpos_start : position;
  lpos_end : position;
}

val loc_from_lexer : lexbuf -> string loc
val loc_from_parser : 'a -> 'a loc
val makeloc : position -> position -> 'a -> 'a loc
val inherit_loc : 'a loc -> 'b -> 'b loc

val pos_formatter : Format.formatter -> position -> unit
val loc_formatter : Format.formatter -> 'a loc -> unit
