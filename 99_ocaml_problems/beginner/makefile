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
	run_length_encoding \
	mod_run_length_encoding \
	duplicate_the_elements_of_a_list \
	split_list_into_two_parts \
	remove_the_kth_element_from_a_list \
	insert_an_element_at_a_given_position_into_a_list \
	create_a_list_containing_all_integers_within_a_given_range \
	lotto_draw_n_different_random_numbers_from_the_set_1_to_m

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
