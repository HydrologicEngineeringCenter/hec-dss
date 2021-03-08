HECLIBS=..\..\heclib\heclib7\heclib_c_v6v7\x64\Debug\heclib_c_v6v7.lib ..\..\heclib\heclib7\heclib_f_v6v7\x64\Debug\heclib_f_v6v7.lib
CFLAGS= -I ..\..\heclib\heclib7\headers /c
OUT=/out:
LINK=link /NODEFAULTLIB:LIBCMTD

all: ts_write.exe ExampleTimeSeries1.exe error.exe  

clean: 
	del *.exe *.dss *.obj *.log

.c.obj:
	cl  $(CFLAGS)  $*.c

ts_write.exe: ts_write.obj
	$(LINK) ts_write.obj  $(HECLIBS)

ExampleTimeSeries1.exe: ExampleTimeSeries1.obj
	$(LINK) ExampleTimeSeries1.obj  $(HECLIBS)

error.exe: error.obj
	$(LINK) error.obj  

