:: Windows Build for Heclib/DSS and javaHeclib
::
call vs_env.bat
echo on

SET DSS_PLATFORM_DIR=x64

:: set version and build_number for javaheclib.dll
set build_number=9999
if not "%1" == "" (set build_number=%1)
echo %build_number%

dssVersion.exe heclib\heclib_c\src\headers\hecdssInternal.h %build_number% > heclib\javaheclib\version_build.h

cd heclib\heclib_f
:: debug
nmake -f Makefile.win  DEBUG=1 clean
nmake -f Makefile.win  DEBUG=1 

:: release
nmake -f Makefile.win  clean
nmake -f Makefile.win  

cd %~dp0
msbuild dss.sln /p:Configuration=Release /p:Platform=x64
if NOT %ERRORLEVEL% == 0  exit /b -1

msbuild dss.sln /p:Configuration=Debug /p:Platform=x64
if NOT %ERRORLEVEL% == 0  exit /b -1

cd %~dp0\test\Fortran
call build.bat

cd %~dp0
del heclib.zip
del javaHeclib.zip

mkdir release64
cd release64
del /f *.dll *.lib
copy ..\heclib\javaHeclib\x64\Release\javaHeclib.dll
copy ..\heclib\hecdss\x64\Release\hecdss.dll
copy ..\heclib\heclib_c\x64\Release\heclib_c.lib 
copy ..\heclib\heclib_f\x64\Release\heclib_f.lib
7z a ..\javaHeclib.zip javaHeclib.dll
7z a ..\hecdss.zip hecdss.dll
cd ..
7z a -tzip heclib.zip release64

mkdir debug64
cd debug64
del /f *.dll *.lib
copy ..\heclib\javaHeclib\x64\Debug\javaHeclib.dll 
copy ..\heclib\hecdss\x64\Debug\hecdss.dll 
copy ..\heclib\heclib_c\x64\Debug\heclib_c.lib 
copy ..\heclib\heclib_f\x64\Debug\heclib_f.lib 
cd ..
7z a -tzip heclib.zip debug64
cd %~dp0
