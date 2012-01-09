.PHONY: clean

GHC=ghc
GHC_OPTS=-package-conf ../../cabal-dev/packages-7.0.4.conf

helloworld: helloworld.hs
	$(GHC) $(GHC_OPTS) --make helloworld

clean:
	rm -f helloworld *.o *.hi
