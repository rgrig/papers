# vim:noet:
MAIN=fool safety biabduction
INP=.:../lib:fig_code:

PDF=$(addsuffix .pdf,$(MAIN))

build: $(PDF)

$(PDF): %.pdf:
	@TEXINPUTS=$(INP) python compile.py "$*"

safety.pdf: safety.tex safety.bib

fool.pdf: fool.tex safety.bib fig_code sigplanconf.cls

biabduction.pdf: biabduction.tex

clean:
	@$(MAKE) -s -C fig_code clean
	@rm -rf *.aux *.bbl *.bak *.blg *.log *.out *.pdf

cleanbib:
	@echo "WARNING: Target NOT tested on Mac."
	@cp safety.bib safety.bib.bak && bibtool safety.bib -r ../lib/bibtool.cfg -o tmp.bib && mv tmp.bib safety.bib; done

fig_code:
	@$(MAKE) -s -C fig_code

sigplanconf.cls:
	@python wget.py http://www.sigplan.org/sigplanconf.cls sigplanconf.cls

.PHONY: req build clean cleanbib fig_code
