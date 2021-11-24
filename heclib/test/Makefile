.SUFFIXES: .f90
UNAME_S := $(shell uname -s)

LIBS=../../heclib/Output/heclib.a 
C_INCL_DIRS=../../heclib/heclib_c/src/headers
F_INCL_DIRS=../../heclib/heclib_f/src/headers

ifeq ($(UNAME_S),Linux)
	CC=gcc
	F90=gfortran
	LD=gfortran
	CWARNING=-Werror
	CFLAGS=-c -m64 -fPIC -g -std=gnu99 -I$(C_INCL_DIRS) $(CWARNING) -D__linux__
	FWARNINGS=-Werror -Warray-bounds -Wc-binding-type -Wcharacter-truncation
	FDEBUG=-fcheck=all,no-recursion
	FFLAGS=-c -g -cpp -m64 -fPIC $(FWARNINGS) $(FDEBUG) -I$(F_INCL_DIRS)  
	LFLAGS=-lm -lz
endif

all: testVerticalDatum_c testVerticalDatum_f

testVerticalDatum_c : testVerticalDatum_c.o
	$(LD) $(LFLAGS) $< -o $@ $(LIBS)
	mv -f $@ Output
	rm $<

testVerticalDatum_f : testVerticalDatum_f.o
	$(LD) $(LFLAGS) $< -o $@ $(LIBS)
	mv -f $@ Output
	rm $<

.c.o:
	$(CC) $(CFLAGS) $< -o $@

.f90.o:
	$(F90) $(FFLAGS) $< -o $@

clean:
	rm -f Output/*
	rm -f *.dss
	rm -f *.mod
	rm -f *.ini
	rm -f vgcore*
