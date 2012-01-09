.PHONY: clean

GHC=ghc
GHC_OPTS=-package-conf ../../cabal-dev/packages-7.0.4.conf

helloworld: helloworld.hs
	$(GHC) $(GHC_OPTS) --make helloworld

hamlet1: hamlet1.hs
	$(GHC) $(GHC_OPTS) --make hamlet1

clean:
	rm -f helloworld hamlet1 *.o *.hi
