TEX := $(filter-out assignmentSetup.tex, $(filter-out bookSetup.tex, $(filter-out setup.tex, $(wildcard *.tex))))
PDF = $(patsubst %.tex, %.pdf, $(TEX))


all: $(PDF)

1g.pdf: $< $(wildcard ../assignments/scratch/*.tex) $(wildcard ../assignments/gettingStarted/*.tex) setup.tex
2i.pdf: $< $(wildcard ../assignments/myFirstFSharp/*.tex) setup.tex
3i.pdf: $< $(wildcard ../assignments/bindings/*.tex) setup.tex
4g.pdf: $< $(wildcard ../assignments/modules/*.tex) $(wildcard ../assignments/vec/*.tex) setup.tex
5i.pdf: $< $(wildcard ../assignments/lister/*.tex) setup.tex
6g.pdf: $< $(wildcard ../assignments/recursion/*.tex) $(wildcard ../assignments/continuedFractions/*.tex) setup.tex
7g.pdf: $< $(wildcard ../assignments/sort/*.tex) $(wildcard ../assignments/weekday/*.tex) $(wildcard ../assignments/imgutil/*.tex) $(wildcard ../assignments/randomText/*.tex) $(wildcard ../assignments/randomWords/*.tex) setup.tex ../assignments/randomText/treeType.pdf
8i.pdf: $< $(wildcard ../assignments/polynomials/*.tex) $(wildcard ../assignments/integration/*.tex) $(wildcard ../assignments/exceptions/*.tex) $(wildcard ../assignments/treeStructure/*.tex) setup.tex
9i.pdf: $< $(wildcard ../assignments/exceptions/*.tex) $(wildcard ../assignments/io/*.tex) $(wildcard ../assignments/web/*.tex) $(wildcard ../assignments/cat/*.tex) assignmentSetup.tex bookSetup.tex
10i.pdf: $< $(wildcard ../assignments/classes/*.tex) assignmentSetup.tex bookSetup.tex
11g.pdf: $< $(wildcard ../assignments/design/*.tex) $(wildcard ../assignments/inheritance/*.tex) $(wildcard ../assignments/uml/*.tex) $(wildcard ../assignments/ricochet-robots/*.tex) assignmentSetup.tex bookSetup.tex
12g.pdf: $< $(wildcard ../assignments/inheritance/*.tex) $(wildcard ../assignments/chess/*.tex) setup.tex
13i.pdf: $< $(wildcard ../assignments/winforms/*.tex) $(wildcard ../assignments/clock/*.tex) setup.tex

%.pdf : %.tex
	latexmk -pdf $<
	touch $@ # latexmk does not change the pdf if texs are up-to-date, so to avoid future compiles...
	@#ls -ltrh $^ $@

#$(PDF): FORCE

#FORCE:

#.PHONY: $(PDF)
