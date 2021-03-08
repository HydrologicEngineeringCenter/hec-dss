@echo off
cd %~dp0
 
set A=x64

if  "%1"=="Win32" (
set A=Win32
call  ..\vs_env32.bat
) else (
call  ..\vs_env.bat
)

::msbuild Dss-C\Dss-C.vcxproj
msbuild Dss-C\Dss-C.vcxproj /t:Rebuild /p:Configuration=Release
call test_setup.bat


..\Dss-C\%A%\Release\Dss-C.exe
if NOT %errorlevel% == 0 (
echo Error running Dss-C error level = %errorlevel%
cd ..
exit /b %errorlevel%
)
cd ..
