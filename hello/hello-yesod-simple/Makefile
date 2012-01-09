.PHONY: clean

GHC=ghc
GHC_OPTS=-package-conf ../../cabal-dev/packages-7.0.4.conf

helloworld: helloworld.hs
	$(GHC) $(GHC_OPTS) --make helloworld

hamlet1: hamlet1.hs
	$(GHC) $(GHC_OPTS) --make hamlet1

hamlet2: hamlet2.hs
	$(GHC) $(GHC_OPTS) --make hamlet2

hamlet3: hamlet3.hs
	$(GHC) $(GHC_OPTS) --make hamlet3

clean:
	rm -f helloworld hamlet1 hamlet2 hamlet3 *.o *.hi
