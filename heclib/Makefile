UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

ifeq ($(UNAME_S),Linux)
	CWARNING=-Werror
	ifneq ($(UNAME_M),aarch64)
		BITS=-m64
	endif
	CFLAGS=-c $(BITS) -fPIC -g -std=gnu99 -Isrc/headers $(CWARNING) -D__linux__ -fcommon
	Make_OS=Linux
	MAKE=make
endif
ifeq ($(UNAME_S),SunOS)
	CFLAGS=-m64 -xmemalign=2i -xcode=pic32 -c -Isrc/headers -mt -xcode=abs64 -xstrconst -xc99=all -xCC -g -xs
	Make_OS=Solaris
	MAKE=gmake
endif
ifeq ($(UNAME_S),Darwin)
	CWARNING=-Werror=implicit-function-declaration
	CFLAGS=-m64 -c -fPIC -g -std=gnu99 -Isrc/headers $(CWARNING) -D__darwin__
	Make_OS=MacOS
	MAKE=make
endif
all:
	(cd heclib_c; $(MAKE) )
	(cd heclib_f; $(MAKE) )		
	(mkdir -p Output)
	( rm -f Output/heclib.a)
	# make sure our heclib.a artifact is a fully formed static archive
	(mkdir hec_c; cd hec_c; ar x ../heclib_c/Output/libhec_c.a)
	(mkdir hec_f; cd hec_f; ar x ../heclib_f/Output/libhec_f.a)
	($(AR) -rsc Output/heclib.a hec_c/*.o hec_f/*.o )  
	(cd javaHeclib; $(MAKE) )
	ls -la ./javaHeclib/Output/*javaHeclib.*
	(cd hecdss; $(MAKE) )
	cp ./javaHeclib/Output/*javaHeclib.* Output/
	cp ./hecdss/libhecdss.* Output/
	(cd Output; zip javaHeclib.zip libjavaHeclib.*)
	(cd Output; zip heclib.zip heclib.a )
	(cd Output; zip hecdss.zip libhecdss.* )
	(cd Output; tar -czvf javaHeclib.tar.gz libjavaHeclib.* )
	ls -la Output/
	

clean:
	( rm -f Output/*.a)
	( cd ../nws_shef; $(MAKE) -f Makefile.$(Make_OS) clean)
	(cd heclib_c; $(MAKE) -f Makefile clean)
	(cd heclib_f; $(MAKE) -f Makefile clean)
	(cd javaHeclib; $(MAKE) -f Makefile clean)


test:
	( cd ../test/C; mkdir -p Output; ./unix_test)
	( cd ../test/Dss-C; mkdir -p Output; $(MAKE) clean test )
	( cd ../test/Fortran; mkdir -p Output; $(MAKE) -fMakefile.$(Make_OS) clean test )
