@echo off
echo "================"  
echo "Starting FORTRAN Tests " 
echo "================" 

cd %~dp0
cd Fortran

set A=x64

if "%1"=="Win32" (
	set A=Win32
)
start "build-fortran" /high /wait build.bat
cd ..
call test_setup.bat
..\Fortran\%A%\Debug\Dss7-Fortran.exe
if NOT %errorlevel% == 0 (
echo Error running FORTRAN tests
exit /b %errorlevel% 
)