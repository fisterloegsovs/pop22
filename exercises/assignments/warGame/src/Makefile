
.PHONO: all
all: wargame.out
	cat $<

wargame.out: wargame.exe
	mono $< > $@

wargame.exe: wargame.fs
	fsharpc --nologo $<

.PHONO: clean
clean:
	rm -rf *~ *.out *.exe FSharp.Core.dll
