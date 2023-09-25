# Compiler options
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLFLAGS = # -w A-4-33-40-41-42-44-45

# List of program names (without the .ml extension)
PROGRAMS = construct_completely_balanced_binary_trees \
	       count_the_leaves_of_a_binary_tree
		   
# Targets
all: $(PROGRAMS)

# Compile and execute each program
$(PROGRAMS): %: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $< 
	./$@

clean:
	rm -f *.cmi *.cmo *.cmx *.o $(PROGRAMS)

# PHONY targets (these targets don't represent files)
.PHONY: all clean
