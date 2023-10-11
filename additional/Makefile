# ocamlfind ocamlc -package str -linkpkg 4_frequencies.ml

# Compiler options
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLFLAGS = -w -24

# List of program names (without the .ml extension)
LIBS =

PROGRAMS = 1_a_few_of_chemistry \
2_temperature_conversion_system \
3_matrix_calculi \
4_frequencies \
5_playing_around_with_strings \
6_approximated_trigonometric_functions


# Targets
all: clean $(PROGRAMS)

# Compile and execute each program
$(PROGRAMS): %: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(LIBS) $<
	./$@

clean:
	rm -f *.cmi *.cmo *.cmx *.o $(PROGRAMS)

# PHONY targets (these targets don't represent files)
.PHONY: all clean
