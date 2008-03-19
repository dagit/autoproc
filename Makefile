GHC = ghc

.PHONY : autoproc
autoproc:
	$(GHC) -o autoproc Main.hs --make

clean:
	rm -rf *.o
	rm -rf *.hi

