# Makefile for compiling expressions.ml and new.ml separately

# Compiler options
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLFLAGS = # -w A-4-33-40-41-42-44-45

# Targets
all: expressions 

run: all
	 ./expressions

expressions: expressions.ml
	$(OCAMLC) $(OCAMLFLAGS) -o expressions expressions.ml

run_expressions: expressions
	./expressions


clean:
	rm -f expressions new *.cmi *.cmo *.cmx *.o
