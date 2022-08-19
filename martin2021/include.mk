TARGETLIB= ~/Dropbox/Courses/POP/2021/slides
PDFLATEX=pdflatex -halt-on-error

help:
	@echo "Possible targets:"
	@echo "  make clean  : cleanup directory"
	@echo "  make all    : build lecture slides"

clean:
	rm -rf *~ *.aux *.log *.bbl *.blg *.toc *.out *.vrb *.snb *.nav *.snm *.pdf *-1.png

%.pdf: %.tex ../fsharp.tex ../util.tex Makefile
	$(PDFLATEX) $<
	$(PDFLATEX) $<
	pdftoppm -png -f 1 -l 1 -rx 1024 -ry 1024 -singlefile $@ $*-1
	cp $@ $*-1.png $(TARGETLIB)/
