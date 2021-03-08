@echo off

cd %~dp0
 

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

GridTest -d ..\..\dss-test-data\grid\Windows7_cumulus_ascii2grid_Snow.2017.01.dss  -p "/SHG/RED RIVER NORTH/SWE/01JAN2017:0600//SNODAS/" -v
