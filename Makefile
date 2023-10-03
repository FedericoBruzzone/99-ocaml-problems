# Compiler options
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLFLAGS = -w -24

# List of program names (without the .ml extension)
PROGRAMS = 01 \
	02 \
	03 \
	04 \
	05 \
	06 \
	07 \
	08 \
	09 \
	10 \
	11 \
	12 \

# Targets
all: clean $(PROGRAMS)

# Compile and execute each program
$(PROGRAMS): %: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $<
	./$@

clean:
	rm -f *.cmi *.cmo *.cmx *.o $(PROGRAMS)

# PHONY targets (these targets don't represent files)
.PHONY: all clean
