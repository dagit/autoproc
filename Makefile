GHC = ghc

.PHONY : autoproc
autoproc:
	$(GHC) -o autoproc autoproc.hs --make

clean:
	rm -rf *.o
	rm -rf *.hi

