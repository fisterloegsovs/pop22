SUBDIRS := $(wildcard */.)

all: $(SUBDIRS) main.tex
	latexmk -pdf main
$(SUBDIRS):
	$(MAKE) -C $@

.PHONY: all $(SUBDIRS)
