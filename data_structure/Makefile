.SILENT:
# ocamlfind ocamlc -package str -linkpkg 4_frequencies.ml

# Compiler options
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLFLAGS = -w -24

# List of program names (without the .ml extension)
LIBS =

PROGRAMS = Maybe \
IsMonad

# Targets
all: clean $(PROGRAMS)

# Compile and execute each program
$(PROGRAMS): %: %.ml
	@echo "\e[32mCompiling:\e[0m \e[1m$@\e[0m"
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(LIBS) Monad.mli $@.ml; \

clean:
	rm -f *.cmi *.cmo *.cmx *.o $(PROGRAMS)

# PHONY targets (these targets don't represent files)
.PHONY: all clean

