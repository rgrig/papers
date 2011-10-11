# Configuration
OCFLAGS=-dtypes,-w,a#Ae
MAINS=interp gen instrumenter

# Things that should seldom change
BYTES=$(addsuffix .byte,$(MAINS))
OCAMLBUILD=ocamlbuild -use-ocamlfind -cflags $(OCFLAGS)

byte:
	@$(OCAMLBUILD) $(BYTES)

examples: byte
	@./test.sh

checker:
	@mkdir -p classes
	@javac topl/*.java examples/*.java -Xlint:all -d classes

instrumenter_tests: instrumenter.byte
	@CLASSPATH=instrumentation_tests/ ./instrumenter.byte

clean:
	@$(OCAMLBUILD) -clean
	@rm -f .out
	@rm -rf classes

.PHONY: byte clean examples
