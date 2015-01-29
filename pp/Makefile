OCAMLOPT        := ocamlopt.opt
OCAMLC          := ocamlc.opt
OCAMLDEP        := ocamldep.opt
OCAMLLEX        := ocamllex.opt

OCAMLMAKEFILE	:= ../lib/OCamlMakefile

PATH		:= /usr/local/ocaml/bin:$(PATH)

SOURCES		:= desugar.mli          desugar.ml          \
                   desugar_typed.mli    desugar_typed.ml    \
                   desugar_contexts.mli desugar_contexts.ml \
                   lexer.mll                                \
                   parser.ml                                \
                   syntax.ml

USE_CAMLP4	:= yes

LIBS		:= dynlink camlp4fulllib

RESULT          := formlets

include $(OCAMLMAKEFILE)
