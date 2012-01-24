.PHONY: all clean

GHC=ghc
GHC_OPTS=-package-conf cabal-dev/packages-7.0.4.conf
PROGS=helloworld \
      hamlet1 hamlet2 hamlet3 hamlet4 hamlet5 hamlet5-prod hamlet6 \
      shaketext \
      widget1 widget2 \
      forms1

all: $(PROGS)

%: %.hs
	$(GHC) $(GHC_OPTS) --make $@

hamlet5-prod: hamlet5.hs
	$(GHC) $(GHC_OPTS) -cpp -DPRODUCTION --make hamlet5

clean:
	rm -f $(PROGS) *.o *.hi
