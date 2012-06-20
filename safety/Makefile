# vim:noet:
MAIN=biabduction fool notes popl2012
INP=.:../lib:fig_code:

PDF=$(addsuffix .pdf,$(MAIN))
JAVA=$(wildcard *.java)
JAVATEX=$(patsubst %.java,%.tex,$(JAVA))
JAVACLASS=$(patsubst %.java,%.class,$(JAVA))

build: $(PDF)

popl2012.pdf: popl2012.tex safety.bib sigplanconf.cls FMAtoTOPL.tex IncorrectIteratorUse.tex Completeness.tex

biabduction.pdf: biabduction.tex

fool.pdf: fool.tex safety.bib sigplanconf.cls pygmentize.sty IncorrectIteratorUse.tex Resource.tex SortCompare.tex

notes.pdf: notes.tex

$(PDF): %.pdf: req
	@TEXINPUTS=$(INP) python compile.py "$*"

$(JAVATEX): %.tex: %.java %.class req
	@pygmentize -o $@ $<

$(JAVACLASS): %.class: %.java req
	@javac $< || echo "WARNING: $< does not compile."

clean:
	@rm -f $(PDF)
	@rm -f *.aux *.bbl *.bak *.blg *.log *.out *.class
	@rm -f $(JAVATEX)
	@rm -f req

cleanbib: req
	@echo "WARNING: Target NOT tested on Mac."
	@cp safety.bib safety.bib.bak && bibtool safety.bib -r ../lib/bibtool.cfg -o tmp.bib && mv tmp.bib safety.bib

sigplanconf.cls: req
	@python wget.py http://www.cs.colostate.edu/symposium/sigplanconf.cls sigplanconf.cls

req:
	@(python -V > /dev/null 2> /dev/null && python chk_requirements.py && touch req) || (echo "You need python. Sorry." && exit 1)

.PHONY: build clean cleanbib
