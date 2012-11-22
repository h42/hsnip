HSFLAGS = -fwarn-name-shadowing  -XOverloadedStrings
CLG = $(HSFLAGS) --make -O # -dynamic -threaded -rtsopts

PROGS=jpd

all:$(PROGS)

jpd : jpd.hs
	ghc $(CLG) -o jpd jpd.hs

%.o : %.hs
	ghc $(CLG) -c -o $@ $<

% : %.hs
	ghc $(CLG) -o $@ $<


clean:
		-rm *.hi *.o $(PROGS)
