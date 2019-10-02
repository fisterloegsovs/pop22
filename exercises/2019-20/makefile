TEX := $(filter-out setup.tex, $(wildcard *.tex))
PDF = $(patsubst %.tex, %.pdf, $(TEX))


all: $(PDF)

1g.pdf: $< $(wildcard ../assignments/scratch/*.tex) $(wildcard ../assignments/gettingStarted/*.tex) setup.tex
2i.pdf: $< $(wildcard ../assignments/myFirstFSharp/*.tex) setup.tex
3i.pdf: $< $(wildcard ../assignments/bindings/*.tex) setup.tex
4g.pdf: $< $(wildcard ../assignments/modules/*.tex) $(wildcard ../assignments/vec/*.tex) setup.tex
5i.pdf: $< $(wildcard ../assignments/lister/*.tex) setup.tex
6g.pdf: $< $(wildcard ../assignments/recursion/*.tex) $(wildcard ../assignments/continuedFractions/*.tex) setup.tex
7g.pdf: $< $(wildcard ../assignments/sort/*.tex) $(wildcard ../assignments/weekday/*.tex) $(wildcard ../assignments/imgutil/*.tex) $(wildcard ../assignments/awari/*.tex) setup.tex
8i.pdf: $< $(wildcard ../assignments/polynomials/*.tex) $(wildcard ../assignments/integration/*.tex) $(wildcard ../assignments/exceptions/*.tex) $(wildcard ../assignments/treeStructure/*.tex) setup.tex
9i.pdf: $< $(wildcard ../assignments/exceptions/*.tex) $(wildcard ../assignments/io/*.tex) setup.tex
10i.pdf: $< $(wildcard ../assignments/classes/*.tex) setup.tex
11g.pdf: $< $(wildcard ../assignments/inheritance/*.tex) $(wildcard ../assignments/uml/*.tex) $(wildcard ../assignments/wolvesNMoose/*.tex) setup.tex
12g.pdf: $< $(wildcard ../assignments/inheritance/*.tex) $(wildcard ../assignments/chess/*.tex) setup.tex
13i.pdf: $< $(wildcard ../assignments/winforms/*.tex) $(wildcard ../assignments/clock/*.tex) setup.tex

%.pdf : %.tex
	latexmk -pdf $<
	touch $@ # latexmk does not change the pdf if texs are up-to-date, so to avoid future compiles...
	@#ls -ltrh $^ $@

#$(PDF): FORCE

#FORCE:

#.PHONY: $(PDF)
