# Configuration
CFLAGS=-dtypes,-w,Ae
MAINS=interp gen javaOfProperty instrumenter

# Things that should seldom change
BYTES=$(addsuffix .byte,$(MAINS))
OCAMLBUILD=ocamlbuild -use-ocamlfind -cflags $(CFLAGS)

byte:
	@$(OCAMLBUILD) $(BYTES)

examples: byte
	@./test.sh

checker:
	@mkdir -p classes
	@javac topl/*.java -d classes

checker_tests: checker
	@CLASSPATH=classes java -ea topl.Checker

instrumenter_tests: instrumenter.byte
	@CLASSPATH=instrumentation_tests/ ./instrumenter.byte

clean:
	@$(OCAMLBUILD) -clean
	@rm -f .out
	@rm -rf classes

.PHONY: byte clean examples
