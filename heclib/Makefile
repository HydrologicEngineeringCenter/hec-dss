UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
	CWARNING=-Werror
	CFLAGS=-m64 -c -fPIC -g -std=gnu99 -Isrc/headers $(CWARNING) -D__linux__
	Make_OS=Linux
	MAKE=make
endif
ifeq ($(UNAME_S),SunOS)
	CFLAGS=-m64 -xmemalign=2i -xcode=pic32 -c -Isrc/headers -mt -xcode=abs64 -xstrconst -xc99=all -xCC -g -xs
	Make_OS=Solaris
	MAKE=gmake
endif
all:
	( cd ../nws_shef; $(MAKE) -j -f Makefile.$(Make_OS))
	(cd heclib_c; $(MAKE) )
	(cd heclib_f; $(MAKE) )
	(mkdir -p Output)
	( rm -f Output/heclib.a)
	($(AR) -rcT Output/heclib.a ./heclib_c/Output/libhec_c.a ./heclib_f/Output/libhec_f.a )  
	(cd javaHeclib; $(MAKE) )
	ls -la ./javaHeclib/Output/*javaHeclib.*
	

clean:
	( rm -f Output/*.a)
	( cd ../nws_shef; $(MAKE) -f Makefile.$(Make_OS) clean)
	(cd heclib_c; $(MAKE) -f Makefile clean)
	(cd heclib_f; $(MAKE) -f Makefile clean)
	(cd javaHeclib; $(MAKE) -f Makefile clean)


test:
	( cd ../test/C; mkdir -p Output; ./unix_test)
	( cd ../test/Java; mkdir -p Output; ./run-junit)
	( cd ../test/Dss-C; mkdir -p Output; $(MAKE) -f Makefile.$(Make_OS) clean)
	( cd ../test/Dss-C; mkdir -p Output; $(MAKE) -j -f Makefile.$(Make_OS) test)
