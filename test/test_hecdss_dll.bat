:: "C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Auxiliary\Build\vcvars64.bat"
cd %~dp0


set heclib=..\heclib
set HEADERS=%heclib%\hecdss

::cl -nologo -I%headers% /EHsc .\C\hecdss_test.c ..\heclib\hecdss\x64\Debug\hecdss.lib  /Fe:hecdss_text_test.exe
cl -nologo -I%headers% /EHsc .\C\hecdss_test.c ..\heclib\hecdss\x64\Debug\hecdss.lib 

hecdss_test.exe