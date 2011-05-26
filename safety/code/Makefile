# If other sources are modified then |make| won't recompile properly.
SOURCES=$(wildcard *.ml) $(wildcard *.mll) $(wildcard *.mly)

byte: interp.byte gen.byte

interp.byte gen.byte: $(SOURCES)
	ocamlbuild -cflags -dtypes,-w,Ae gen.byte interp.byte

examples: byte
	@./test.sh

clean:
	ocamlbuild -clean

.PHONY: byte examples clean
