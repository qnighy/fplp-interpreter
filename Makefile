OCAMLMAKEFILE = OCamlMakefile

OCAMLYACC = menhir

SOURCES = misc.mli misc.ml \
	  loc.mli loc.ml \
	  expr.mli expr.ml \
	  value.mli value.ml \
	  type.mli type.ml \
	  thunk.mli thunk.ml \
	  thunk_need.mli thunk_need.ml \
	  eval.mli eval.ml \
	  eval_cbn.mli eval_cbn.ml \
	  eval_need.mli eval_need.ml \
	  exprParser.mly exprLexer.mll \
	  main.ml
RESULT  = main

include $(OCAMLMAKEFILE)
