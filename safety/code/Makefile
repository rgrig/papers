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

checker:
	mkdir -p classes
	javac topl/*.java -d classes

clean:
	$(OCAMLBUILD) -clean
	rm -f .out
	rm -rf classes

.PHONY: byte clean examples
