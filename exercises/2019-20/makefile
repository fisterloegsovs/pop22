TEX := $(filter-out setup.tex, $(wildcard *.tex))
PDF = $(patsubst %.tex, %.pdf, $(TEX))

all: $(PDF)

$(PDF): FORCE

FORCE:

%.pdf : %.tex setup.tex
	latexmk -pdf $<

.PHONY: all
