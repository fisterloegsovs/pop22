
.PHONO: all
all: solitaire.out
	cat $<

solitaire.out: solitaire.exe
	mono $< > $@

solitaire.exe: solitaire.fs
	fsharpc --nologo $<

.PHONO: clean
clean:
	rm -rf *~ *.out *.exe FSharp.Core.dll
