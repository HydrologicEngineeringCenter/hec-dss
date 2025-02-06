@echo off
echo "================"  
echo "Starting FORTRAN Tests " 
echo "================" 

cd %~dp0

call test_setup.bat
..\Fortran\x64\Release\Dss7-Fortran.exe
if NOT %errorlevel% == 0 (
echo Error running FORTRAN tests
exit /b %errorlevel% 
)