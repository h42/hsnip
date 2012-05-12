HSFLAGS = -fwarn-name-shadowing  -XOverloadedStrings
CLG = $(HSFLAGS) -dynamic --make -O2

PROGS=either

all:$(PROGS)

% : %.hs
	ghc $(CLG) -o $@ $<

clean:
		-rm *.hi *.o $(PROGS)
