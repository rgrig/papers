# Configuration
CFLAGS=-dtypes,-w,Ae
MAINS=interp gen javaOfProperty

# Things that should seldom change
BYTES=$(addsuffix .byte,$(MAINS))
OCAMLBUILD=ocamlbuild -cflags $(CFLAGS)

byte:
	@$(OCAMLBUILD) $(BYTES)

examples: byte
	@./test.sh

checker:
	@mkdir -p classes
	@javac topl/*.java -d classes

checker_tests: checker
	@CLASSPATH=classes java -ea topl.Checker

clean:
	@$(OCAMLBUILD) -clean
	@rm -f .out
	@rm -rf classes

.PHONY: byte clean examples
