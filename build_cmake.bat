:: Windows Build for Heclib/DSS and javaHeclib using CMake
::
cd /d %~dp0
::call "%~dp0vs_env.bat"
echo on

SET DSS_PLATFORM_DIR=x64
SET BUILD_DIR=build

:: set version and build_number for javaheclib.dll
set build_number=9999
if not "%1" == "" (set build_number=%1)
echo %build_number%

dssVersion.exe heclib\heclib_c\src\headers\hecdssInternal.h %build_number% > heclib\javaheclib\version_build.h


cd /d %~dp0

:: Clean previous build outputs so we get a fresh build
if exist %BUILD_DIR% rmdir /s /q %BUILD_DIR%
if exist release64 rmdir /s /q release64
if exist debug64 rmdir /s /q debug64
if exist heclib.zip del /f /q heclib.zip
if exist javaHeclib.zip del /f /q javaHeclib.zip
if exist hecdss.zip del /f /q hecdss.zip

:: Configure (matches .github/workflows/build-and-test.yml; works with older CMake
:: that does not support CMakePresets.json "version": 9).
cmake -B %BUILD_DIR% -S .
if NOT %ERRORLEVEL% == 0  exit /b -1

:: Build Release
cmake --build %BUILD_DIR% --config Release
if NOT %ERRORLEVEL% == 0  exit /b -1

:: Build Debug
cmake --build %BUILD_DIR% --config Debug
if NOT %ERRORLEVEL% == 0  exit /b -1

:: Run tests (Debug, matches .github/workflows/build-and-test.yml)
ctest --test-dir %BUILD_DIR% -C Debug --output-on-failure
if NOT %ERRORLEVEL% == 0  exit /b -1

cd /d %~dp0

mkdir release64
cd release64
copy ..\%BUILD_DIR%\heclib\javaHeclib\Release\javaHeclib.dll
copy ..\%BUILD_DIR%\heclib\hecdss\Release\hecdss.dll
copy ..\heclib\hecdss\hecdss.h
copy ..\%BUILD_DIR%\heclib\heclib_c\Release\heclib.lib
7z a ..\javaHeclib.zip javaHeclib.dll
7z a ..\hecdss.zip hecdss.dll hecdss.h
cd ..
7z a -tzip heclib.zip release64

mkdir debug64
cd debug64
copy ..\%BUILD_DIR%\heclib\javaHeclib\Debug\javaHeclib.dll
copy ..\%BUILD_DIR%\heclib\hecdss\Debug\hecdss.dll
copy ..\%BUILD_DIR%\heclib\heclib_c\Debug\heclib.lib
cd ..
7z a -tzip heclib.zip debug64
cd %~dp0
