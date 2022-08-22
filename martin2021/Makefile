
DIRS=`ls -1d lecture_*`

#
# Clean all lecture_*/ folders and the src/ folder
#
.PHONY: clean
clean:
	rm -rf *~ */*~ */*/*~
	rm -rf *.exe */*.exe */*/*.exe
	make -C src clean
	for d in $(DIRS); do make -C $${d} clean; done

#
# Build all slide sets in the lecture_*/ folders. Also produce a
# picture of the first slide to use as a background png for zoom
# recordings.
#
.PHONY: all
all:
	for d in $(DIRS); do make -C $${d} all; done

#
# Build and test source code in the src/ folder
#
.PHONY: test
test:
	make -C src
