HSFLAGS = -fwarn-name-shadowing  -XOverloadedStrings
CLG = $(HSFLAGS) -dynamic --make -O

PROGS=utf8

all:$(PROGS)

% : %.hs
	ghc $(CLG) -o $@ $<

clean:
		-rm *.hi *.o $(PROGS)
