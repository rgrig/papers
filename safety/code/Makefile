byte:
	ocamlbuild -cflag -annot gen.byte interp.byte

examples: byte
	@./test.sh

clean:
	ocamlbuild -clean

.PHONY: byte examples clean
