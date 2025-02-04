cd %~dp0

call  ..\..\vs_env.bat 

@echo on

set OUT_DIR=x64\Debug
set TARGET=%OUT_DIR%\Dss7-Fortran.exe

dir/b/s *.f90 > source.txt

if NOT EXIST %OUT_DIR% mkdir %OUT_DIR%

ifort /nologo /debug:Full /MP /Od /fpp /I"src\headers" /reentrancy:threaded /warn:noalignments ^
 /Qsave /Qinit:zero /names:lowercase /iface:cref /assume:underscore ^
 /traceback /check:all /libs:static /dbglibs /threads  ^
 @source.txt /link ^
 /NODEFAULTLIB:libcmt /NODEFAULTLIB:MSVCRT ^
 ..\..\heclib\heclib_c\%OUT_DIR%\heclib_c.lib ^
 ..\..\heclib\heclib_f\%OUT_DIR%\heclib_f.lib ^
 ..\..\lib\x64\zlibstatic.lib ^
 /out:%TARGET%


del *.obj

set OUT_DIR=x64\Release
set TARGET=%OUT_DIR%\Dss7-Fortran.exe
if NOT EXIST %OUT_DIR% mkdir %OUT_DIR%

ifort /nologo /debug:full /MP /Od /fpp /I"src\headers" /reentrancy:threaded /warn:noalignments ^
 /Qsave /Qinit:zero /names:lowercase /iface:cref /assume:underscore ^
 /traceback /check:all /libs:static /threads  ^
 @source.txt /link /NODEFAULTLIB:libcmt  ^
 ..\..\heclib\heclib_c\%OUT_DIR%\heclib_c.lib ^
 ..\..\heclib\heclib_f\%OUT_DIR%\heclib_f.lib ^
 ..\..\lib\x64\zlibstatic.lib ^
 /out:%TARGET%

del *.obj