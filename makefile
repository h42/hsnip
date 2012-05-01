HSFLAGS = -fwarn-name-shadowing  -XOverloadedStrings
CLG = $(HSFLAGS) -dynamic --make -O2

PROGS=map2

all:$(PROGS)

% : %.hs
	ghc $(CLG) -o $@ $<

clean:
		-rm *.hi *.o $(PROGS)
