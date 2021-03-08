:: Build script for Heclib and javaHeclib
::
call vs_env32.bat
echo on 

:: set version and build_number for javaheclib.dll
set build_number=9999
if not "%1" == "" (set build_number=%1)
echo %build_number%

dssVersion.exe heclib\headers\hecdssInternal.h %build_number% > heclib\javaheclib\version_build.h

devenv dss.sln  /ReBuild "Release|Win32"

 