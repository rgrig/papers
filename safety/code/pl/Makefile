all:
	ocamlbuild -cflag -annot -use-menhir -yaccflag --explain interp.byte gen.byte

examples:
	@for f in examples/*.p; do echo ===$$f===; ./gen.byte 0 | ./interp.byte $$f; done

clean:
	ocamlbuild -clean

.PHONY: all examples clean
