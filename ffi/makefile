HSFLAGS = -O -fwarn-name-shadowing -i/home/jerry/haskell/
CLG = $(HSFLAGS) -dynamic --make
CFLAGS=-Wall -O

PROGS = hsc

% : %.hs
	ghc $(CLG) -o $@ $<

all:$(PROGS)

hsc_c:hsc_c.c

hsc.hs:hsc.hsc hsc_c
	awkinc <hsc_c.c >hsc_c.h
	hsc2hs hsc.hsc

prog1:prog1.hs prog1lib.o
	ghc -o prog1 $(CLG) prog1.hs prog1lib.o

prog2:prog2.hs prog2lib.o
	ghc -o prog2 $(CLG) prog2.hs prog2lib.o

hmat:hmat.hs hmat_c.o
	ghc -o hmat $(CLG) hmat.hs hmat_c.o

hmat_c.o:hmat_c.c

clean:
	-rm *.hi *.o $(PROGS)
