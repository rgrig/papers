# vim:noet:
MAIN=mc
INP=.:../lib:

build:
	@TEXINPUTS=$(INP) BSTINPUTS=$(INP) rubber --force --pdf $(MAIN)

clean:
	@TEXINPUTS=$(INP) BSTINPUTS=$(INP) rubber --clean --pdf $(MAIN)

cleanbib:
	cp $(MAIN).bib $(MAIN).bib.bak
	bibtool -r ../lib/bibtool.cfg -o tmp.bib && mv tmp.bib $(MAIN).bib
