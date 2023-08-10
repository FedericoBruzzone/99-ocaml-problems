# Compiler options
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLFLAGS = # -w A-4-33-40-41-42-44-45

# List of program names (without the .ml extension)
PROGRAMS = tail_of_a_list \
	last_two_elements_of_a_list \
	nth_element_of_a_list \
	length_of_a_list \
	reverse_a_list \
	palindrome \
	run_length_encoding

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
