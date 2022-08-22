
FSFILES=5g.fs exists.fs headOr0.fs alist.fs arr_square.fs		\
 arr_summation.fs bad_summation.fs badrev.fs better_summation.fs	\
 bsearch.fs bsort.fs bsort2.fs closure.fs conf.fs cstest.fs eq.fs	\
 eval.fs evenodd.fs exn.fs exnres.fs expr.fs extract.fs fac2.fs		\
 fact.fs fetchpage.fs fib.fs fibfile.fs find.fs flist.fs fstream.fs	\
 gen_msort.fs gen_ssort.fs good_summation.fs hanoi2.fs hanoi_old.fs	\
 isort.fs len.fs levensthein.fs main.fs map.fs msort.fs mss.fs		\
 mss_lists.fs multabel.fs pegs.fs person.fs pp.fs qsort.fs regex.fs	\
 rev.fs ssort.fs ssort_cmp.fs stest.fs str.fs total.fs trees.fs		\
 ugly_summation.fs

# fetchfx.fs gtk_turtle.fs

OUTFILES=$(FSFILES:%.fs=%.out)

.PHONY: all
all: $(OUTFILES) queue_good queue_bad latex/test.pdf

%.exe: %.fs pegs.dll
	fsharpc --nologo -r pegs.dll $<

%.out: %.exe
	mono $< > $@
	cat $@

.PHONY: clean
clean:
	rm -rf *.exe *.out *.dll *~ latex/test.* latex/*~

%.dll: %.fsi %.fs
	fsharpc --nologo -a $^

.PHONY: queue_good
queue_good:
	cp queue_good.fs queue.fs
	fsharpc --nologo -a queue.fsi queue.fs
	fsharpc --nologo -r queue.dll qtest.fs
	time mono qtest.exe

.PHONY: queue_bad
queue_bad:
	cp queue_bad.fs queue.fs
	fsharpc --nologo -a queue.fsi queue.fs
	fsharpc --nologo -r queue.dll qtest.fs
	time mono qtest.exe

latex/test.tex: expr.exe
	mono expr.exe > latex/test.tex

latex/test.pdf: latex/test.tex
	(cd latex; pdflatex test.tex)
