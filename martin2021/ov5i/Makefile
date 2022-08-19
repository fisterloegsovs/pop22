

all: ov0 ov1 ov2 ov3 ov4 ov5 ov6 ov7 ov8 ov9 ov10 ov11 ov12 ov13 op1 op2

o%: o%.exe
	mono $<

o%.exe: o%.fs
	fsharpc $<

clean:
	rm -f *~ *.exe
