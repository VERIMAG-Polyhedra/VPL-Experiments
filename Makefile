OCAMLDOC_FLAGS = -hide-warnings
SRC = src
OCB_FLAGS = -cflags "-g -w "+a-4-32" -warn-error "+a-4-32"" -lflag -g -use-ocamlfind $(INCLUDES)

OCB = ocamlbuild -I $(SRC) $(OCB_FLAGS)

all: vpl

clean:
	$(OCB) -clean
	rm -f Run_VPL.byte

# check that packages can be found
sanity:
	ocamlfind query zarith
	ocamlfind query unix
	ocamlfind query FrontC
	ocamlfind query vpl-core

vpl : sanity
	$(OCB) -package vpl-core -package FrontC Run_VPL.byte

newpolka : sanity
	$(OCB) -package zarith -package apron -package apron.polkaMPQ -package FrontC Run_Newpolka.byte

.PHONY: clean exp sanity
