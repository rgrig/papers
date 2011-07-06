# vim:noet:
MAIN=safety fool
INP=.:../lib:fig_code:

build: fig_code
	@TEXINPUTS=$(INP) BSTINPUTS=$(INP) rubber -W all --force --pdf $(MAIN)

clean:
	@TEXINPUTS=$(INP) BSTINPUTS=$(INP) rubber --clean --pdf $(MAIN)
	$(MAKE) -s -C fig_code clean

cleanbib:
	cp $(MAIN).bib $(MAIN).bib.bak
	bibtool -r ../lib/bibtool.cfg -o tmp.bib && mv tmp.bib $(MAIN).bib

req: sigplan.cls

fig_code:
	$(MAKE) -s -C fig_code

sigplan.cls:
	wget http://www.sigplan.org/sigplanconf.cls

.PHONY: req build clean cleanbib fig_code
