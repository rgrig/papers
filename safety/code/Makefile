# Configuration
CFLAGS=-dtypes,-w,Ae
MAINS=interp gen

# Things that should seldom change
BYTES=$(addsuffix .byte,$(MAINS))
OCAMLBUILD=ocamlbuild -cflags $(CFLAGS)

byte:
	$(OCAMLBUILD) $(BYTES)

examples: byte
	@./test.sh

clean:
	$(OCAMLBUILD) -clean
	rm -f .out

.PHONY: byte clean examples
