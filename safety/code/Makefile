# If other sources are modified then |make| won't recompile properly.
SOURCES=$(wildcard *.ml) $(wildcard *.mll) $(wildcard *.mly)
CFLAGS=-dtypes,-w,a#Ae

byte: interp.byte gen.byte instrumenter.byte

interp.byte gen.byte instrumenter.byte: $(SOURCES)
	OCAMLPATH=~/lib/ocaml/baristaLibrary ocamlbuild -cflags $(CFLAGS) gen.byte interp.byte instrumenter.byte

examples: byte
	@./test.sh

clean:
	ocamlbuild -clean
	rm -f .out

.PHONY: byte examples clean
