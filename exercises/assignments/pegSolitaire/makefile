TEX := $(wildcard *.tex)

main.pdf: $(TEX)
	pdflatex main

.PHONY:
clean:
	rm -rf *~ *.out *.aux *.log *.idx
