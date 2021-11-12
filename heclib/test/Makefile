UNAME_S := $(shell uname -s)

LIBS=../../heclib/Output/heclib.a 
INCL_DIRS=../../heclib/heclib_c/src/headers

ifeq ($(UNAME_S),Linux)
	CC=gcc
	LD=gfortran
	CWARNING=-Werror
	CFLAGS=-c -m64 -fPIC -g -std=gnu99 -I$(INCL_DIRS) $(CWARNING) -D__linux__
	LFLAGS=-lm -lz
endif

all: testVerticalDatum7

testVerticalDatum7 : testVerticalDatum7.o
	$(LD) $(LFLAGS) $< -o $@ $(LIBS)
	mv -f $@ Output
	rm $<

.c.o:
	cc $(CFLAGS) $< -o $@

clean:
	rm -f Output/*
	rm -f *.dss