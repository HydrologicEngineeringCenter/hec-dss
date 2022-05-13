:: Build script for Heclib and javaHeclib
::
call vs_env.bat
echo on

:: set version and build_number for javaheclib.dll
set build_number=9999
if not "%1" == "" (set build_number=%1)
echo %build_number%

dssVersion.exe heclib\heclib_c\src\headers\hecdssInternal.h %build_number% > heclib\javaheclib\version_build.h


devenv dss.sln  /ReBuild  "Debug|x64"
::devenv dss.sln  /ReBuild  "Release|x64"

::call vs_env32.bat
::devenv dss.sln  /ReBuild "Release|Win32"
::devenv dss.sln  /ReBuild  "Debug|Win32"
  

