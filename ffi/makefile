HSFLAGS = -O -fwarn-name-shadowing -i/home/jerry/haskell/lib
CLG = $(HSFLAGS) -dynamic --make
CFLAGS=-Wall

PROGS=prog1 prog2

% : %.hs
	ghc $(CLG) -o $@ $<

all:$(PROGS)

prog1:prog1.hs prog1lib.o
	ghc -o prog1 $(CLG) prog1.hs prog1lib.o

prog2:prog2.hs prog2lib.o
	ghc -o prog2 $(CLG) prog2.hs prog2lib.o

clean:
	-rm *.hi *.o $(PROGS)
