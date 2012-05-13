CFLAGS=-Wall

VERSION=1
MINOR=0
REL=1
NAME=dclib
LINKNAME=lib$(NAME).so
SONAME=$(LINKNAME).$(VERSION)
FULLNAME= $(SONAME).$(MINOR).$(REL)

jpd:jpd.c $(SONAME)

$(SONAME):dclib.o
	gcc -shared -Wl,-soname,$(SONAME) -o $(FULLNAME) dclib.o -lc
	rm -f $(SONAME) $(LINKNAME)
	ln -s $(FULLNAME) $(SONAME)
	ln -s $(FULLNAME) $(LINKNAME)

#dclib.o:dclib.c
#        gcc -c $(CFLAGS) dclib.c

clean:
	-rm *.o  *.so  *.so.* jpd
