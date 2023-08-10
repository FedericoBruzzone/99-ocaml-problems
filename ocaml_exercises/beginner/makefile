# Makefile for compiling expressions.ml and new.ml separately

# Compiler options
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLFLAGS = # -w A-4-33-40-41-42-44-45

# Targets
all: tail_of_a_list \
	last_two_elements_of_a_list \
	nth_element_of_a_list 

tail_of_a_list: tail_of_a_list.ml
	$(OCAMLC) $(OCAMLFLAGS) -o tail_of_a_list tail_of_a_list.ml 
	./tail_of_a_list

last_two_elements_of_a_list: last_two_elements_of_a_list.ml
	$(OCAMLC) $(OCAMLFLAGS) -o last_two_elements_of_a_list last_two_elements_of_a_list.ml
	./last_two_elements_of_a_list

nth_element_of_a_list: nth_element_of_a_list.ml
	$(OCAMLC) $(OCAMLFLAGS) -o nth_element_of_a_list nth_element_of_a_list.ml
	./nth_element_of_a_list

clean:
	rm -f *.cmi *.cmo *.cmx *.o tail_of_a_list last_two_elements_of_a_list nth_element_of_a_list

# PHONY targets (these targets don't represent files)
.PHONY: all clean
