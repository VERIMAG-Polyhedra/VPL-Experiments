OCAMLDOC_FLAGS = -hide-warnings
SRC = src
OCB_FLAGS = -cflag -g -lflag -g -use-ocamlfind $(INCLUDES)

OCB = ocamlbuild -I $(SRC) $(OCB_FLAGS)

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
	$(OCB) -package oclock -package vpl -package FrontC Run_VPL.byte

.PHONY: clean exp sanity
