echo "Hi"
cd %~dp0 

set SRC="%IFORT_COMPILER19%redist\intel64_win\compiler\"
set DEST=x64\Debug\

copy %SRC%libifcoremdd.dll %DEST%
copy %SRC%libifportmd.dll %DEST%
copy %SRC%libmmd.dll %DEST%
copy %SRC%libmmdd.dll %DEST%
copy %SRC%ucrtbased.dll %DEST%
copy %SRC%vcruntime140d.dll %DEST%
copy "C:\Program Files (x86)\Microsoft SDKs\Windows Kits\10\ExtensionSDKs\Microsoft.UniversalCRT.Debug\10.0.17763.0\Redist\Debug\x64\ucrtbased.dll"  %DEST%


dir %DEST%


set SRC="%IFORT_COMPILER19%redist\ia32_win\compiler\"
set DEST=Win32\Debug\

copy %SRC%libifcoremdd.dll %DEST%
copy %SRC%libifportmd.dll %DEST%
copy %SRC%libmmd.dll %DEST%
copy %SRC%libmmdd.dll %DEST%
copy %SRC%ucrtbased.dll %DEST%
copy %SRC%vcruntime140d.dll %DEST%
copy "C:\Program Files (x86)\Microsoft SDKs\Windows Kits\10\ExtensionSDKs\Microsoft.UniversalCRT.Debug\10.0.17763.0\Redist\Debug\x86\ucrtbased.dll"  %DEST%

dir %DEST%
