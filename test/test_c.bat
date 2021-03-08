@echo off

cd %~dp0
 
set /a counter=0
set /a fail=0

set A=x64

if  "%1"=="Win32" (
set A=Win32
call  ..\vs_env32.bat
) else (
call  ..\vs_env.bat
)

call test_setup.bat
 
echo "================"  
echo "Starting C Tests " 
echo "================" 

set code=..\C
set heclib=..\..\heclib
set libs=%heclib%\heclib_c_v6v7\%A%\Release\heclib_c_v6v7.lib %heclib%\heclib_f_v6v7\%A%\Release\heclib_f_v6v7.lib
set DSS_LINK=/NODEFAULTLIB:LIBCMTD  /LIBPATH:"C:\Program Files (x86)\IntelSWTools\compilers_and_libraries_2019.3.203\windows\compiler\lib\intel64_win\" "kernel32.lib" "user32.lib" "gdi32.lib" "winspool.lib" "comdlg32.lib" "advapi32.lib" "shell32.lib" "ole32.lib" "oleaut32.lib" "uuid.lib" "odbc32.lib" "odbccp32.lib" /DEBUG /MACHINE:X64 /NODEFAULTLIB:LIBCMT

cl /c -nologo -I%heclib%\headers   /Debug /ZI  %code%\getopt.c  
cl -nologo -I%heclib%\headers   /Debug /ZI   %code%\GridTest.c ..\..\lib\%A%\zlibstatic.lib getopt.obj %libs% /link  %DSS_LINK%
cl -nologo -I%heclib%\headers /w  /MT /Debug /ZI   %code%\CatalogTest.c %libs% /link  %DSS_LINK%
cl -nologo -I%heclib%\headers   /Debug /ZI   %code%\endian.c %libs% /link  %DSS_LINK%

endian
CatalogTest v6-pc.dss
CatalogTest v7-pc.dss

call :test SamplePairedDataDoubles
call :test SamplePairedData6
call :test ts_readv6  
call :test ts_write_irregular
call :test SampleText1
call :test ExampleSecondGranularity
call :test SamplePairedData
call :test ts_write
call :test ExampleTimeSeries1
call :test ExampleTimeSeries2
call :test zopenExample
call :test ExampleMinuteGranularity

echo "Running Grid Tests"

GridTest.exe -d gridtest.dss -h 10 -w 5 -c 1 -r 12   
if NOT %errorlevel% == 0 (
echo Error running GridTest
set /a %fail+=1
)
GridTest.exe -d gridtest.dss -v   
if NOT %errorlevel% == 0 (
echo Error running GridTest/verification
set /a %fail+=1
)

echo "ran %counter% tests and some grid test stuff."
echo "%fail% tests FAILED"

if NOT %fail% ==0 (
echo "exiting with error"
 exit /b -1
)
 
goto :eof

::=============================
::  function to test and run
::============================
:test
echo %1
::Compile
cl -nologo -I%heclib%\headers   /Debug /ZI  %code%\%1.c %libs% /link  %DSS_LINK%  

::Run
%1.exe 
if NOT %errorlevel% == 0 (
    echo Error running %1
    set /a %fail+=1
    )
set /a %counter+=1
goto :eof
