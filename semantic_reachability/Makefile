# vim:noet:
MAIN=semantic_reachability
INP=.:../lib:

build:
	@TEXINPUTS=$(INP) BSTINPUTS=$(INP) rubber --force --pdf $(MAIN)

clean:
	@TEXINPUTS=$(INP) BSTINPUTS=$(INP) rubber --clean --pdf $(MAIN)

cleanbib:
	cp phd.bib phd.bib.bak
	bibtool -r bibtool.cfg -o tmp.bib && mv tmp.bib phd.bib
