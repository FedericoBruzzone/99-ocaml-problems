# Compiler options
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLFLAGS = # -w A-4-33-40-41-42-44-45

# List of program names (without the .ml extension)
PROGRAMS = determine_whether_two_positive_integer_numbers_are_coprime \
		   compare_the_two_methods_of_calculating_eulers_totient_function \
		   a_list_of_prime_numbers

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
