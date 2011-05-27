# If other sources are modified then |make| won't recompile properly.
SOURCES=$(wildcard *.ml) $(wildcard *.mll) $(wildcard *.mly)
CFLAGS=-dtypes,-w,Ae

byte: interp.byte gen.byte

interp.byte gen.byte: $(SOURCES)
	ocamlbuild -cflags $(CFLAGS) gen.byte interp.byte

examples: byte
	@./test.sh

clean:
	ocamlbuild -clean
	rm -f .out

.PHONY: byte examples clean
