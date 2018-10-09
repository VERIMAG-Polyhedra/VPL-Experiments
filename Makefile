OCAMLDOC_FLAGS = -hide-warnings
SRC = src
OCB_FLAGS = -cflags "-g -w "+a-4-32" -warn-error "+a-4-32"" -lflag -g -use-ocamlfind $(INCLUDES)

OCB = ocamlbuild -I $(SRC) $(OCB_FLAGS)

MPICC=mpicc
MPIRUN=mpirun

all: exp

clean:
	$(OCB) -clean

# check that packages can be found
sanity:
	ocamlfind query zarith
	ocamlfind query unix
	ocamlfind query FrontC
#	ocamlfind query mpi

vpl : sanity
	$(OCB) -package vpl -package FrontC Run_VPL.byte

newpolka : sanity
	$(OCB) -package zarith -package apron -package apron.polkaMPQ -package FrontC Run_Newpolka.byte

run_mpi : sanity
	$(MPIRUN) -np 4 ./Run_VPL.byte -file tests/test_proj.c -folder benchs/ -proj plp

.PHONY: clean exp sanity
