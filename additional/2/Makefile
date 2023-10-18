.SILENT:
# ocamlfind ocamlc -package str -linkpkg 4_frequencies.ml

# Compiler options
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLFLAGS = -w -24

# List of program names (without the .ml extension)
LIBS =

PROGRAMS = 1_social_networks \


# Targets
all: clean $(PROGRAMS)

# Compile and execute each program
$(PROGRAMS): %: %.ml
	@echo "\e[32mCompiling and executing:\e[0m \e[1m$@\e[0m"
	if [ $@ = "7_playing_around_with_arithmetic" ]; then \
		$(OCAMLC) $(OCAMLFLAGS) -o $@ $(LIBS) MyStack.ml $@.ml; \
	else \
		$(OCAMLC) $(OCAMLFLAGS) -o $@ $(LIBS) $<; \
	fi
	./$@

clean:
	rm -f *.cmi *.cmo *.cmx *.o $(PROGRAMS)

# PHONY targets (these targets don't represent files)
.PHONY: all clean
