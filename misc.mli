(* misc.mli : misc *)

type identifier = string

module IdentifierMap : Map.S with type key = identifier
