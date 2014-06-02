OCAMLMAKEFILE = OCamlMakefile

OCAMLYACC = menhir

SOURCES = loc.mli loc.ml \
	  expr.mli expr.ml \
	  value.mli value.ml \
	  type.mli type.ml \
	  eval.mli eval.ml \
	  exprParser.mly exprLexer.mll \
	  main.ml
RESULT  = main

include $(OCAMLMAKEFILE)
