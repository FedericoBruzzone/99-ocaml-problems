.SILENT:
# ocamlfind ocamlc -package str -linkpkg 4_frequencies.ml

# Compiler options
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLFLAGS = -w -24

# List of program names (without the .ml extension)
LIBS =

PROGRAMS = MakeMonad \
Maybe \
Opt \
Main \
ListM

# Targets
all: clean $(PROGRAMS)

# Compile and execute each program
$(PROGRAMS): %: %.ml
	@echo "\e[32mCompiling:\e[0m \e[1m$@\e[0m"
	if [ $@ = "ListM" ]; then \
		$(OCAMLC) $(OCAMLFLAGS) -o $@ $(LIBS) MakeMonad.ml $@.ml; \
	elif [ $@ = "Main" ]; then \
		$(OCAMLC) $(OCAMLFLAGS) -o $@ $(LIBS) MakeMonad.ml Maybe.ml Opt.ml $@.ml; \
	elif [ $@ = "MakeMonad" ]; then \
		$(OCAMLC) $(OCAMLFLAGS) -o $@ $(LIBS) IMonad.mli $@.ml; \
	elif [ $@ = "Opt" ]; then \
		$(OCAMLC) $(OCAMLFLAGS) -o $@ $(LIBS) MakeMonad.ml Maybe.ml $@.ml; \
	else \
		$(OCAMLC) $(OCAMLFLAGS) -o $@ $(LIBS) $<; \
	fi

	# $(OCAMLC) $(OCAMLFLAGS) -o $@ $(LIBS) IMonad.mli $@.ml; \

clean:
	rm -f *.cmi *.cmo *.cmx *.o $(PROGRAMS)

# PHONY targets (these targets don't represent files)
.PHONY: all clean

