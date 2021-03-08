


if not exist ".\bin\" mkdir bin
del /Q bin\*.*

cd bin
copy ..\..\dss-test-data\*.* .
copy ..\..\dss-test-data\shef\*.* .
dir
cd 