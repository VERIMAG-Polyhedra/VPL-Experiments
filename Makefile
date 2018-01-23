OCAMLDOC_FLAGS = -hide-warnings
OCB_FLAGS = -use-ocamlfind $(INCLUDES)

OCB = ocamlbuild  $(OCB_FLAGS)

all: exp

clean:
	$(OCB) -clean

# check that packages can be found
sanity:
	ocamlfind query zarith
	ocamlfind query unix
	ocamlfind query vpl
	ocamlfind query FrontC
	ocamlfind query apron

exp : sanity
	$(OCB) -package oclock -package apron -package apron.polkaMPQ -package vpl -package FrontC Test_parse.native

.PHONY: clean exp sanity
