.PHONY: clean

GHC=ghc
GHC_OPTS=-package-conf ../cabal-dev/packages-7.0.4.conf

helloworld: helloworld.hs
	$(GHC) $(GHC_OPTS) --make helloworld

hamlet1: hamlet1.hs
	$(GHC) $(GHC_OPTS) --make hamlet1

hamlet2: hamlet2.hs
	$(GHC) $(GHC_OPTS) --make hamlet2

hamlet3: hamlet3.hs
	$(GHC) $(GHC_OPTS) --make hamlet3

hamlet4: hamlet4.hs
	$(GHC) $(GHC_OPTS) --make hamlet4

hamlet5: hamlet5.hs
	$(GHC) $(GHC_OPTS) --make hamlet5

hamlet5-prod: hamlet5.hs
	$(GHC) $(GHC_OPTS) -cpp -DPRODUCTION --make hamlet5

hamlet6: hamlet6.hs
	$(GHC) $(GHC_OPTS) --make hamlet6

shaketext: shaketext.hs
	$(GHC) $(GHC_OPTS) --make shaketext

widget1: widget1.hs
	$(GHC) $(GHC_OPTS) --make widget1

clean:
	rm -f helloworld hamlet1 hamlet2 hamlet3 hamlet4 hamlet5 hamlet6 shaketext widget1 *.o *.hi
