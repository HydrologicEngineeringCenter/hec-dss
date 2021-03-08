:: Test 
cd %~dp0

if not exist ".\bin\" mkdir bin
cd bin

copy ..\..\dss-test-data\*.* .

set JAR=..\..\jar
set CP=%jar%\hec.jar;%jar%\hecData.jar;%jar%\heclib.jar;%jar%\rma.jar;%jar%\hec-dssvue-dev.jar;%jar%\hecPlots-v1.1-dev.jar;.
set JLIB=-Djava.library.path=..\..\heclib\javaHeclib\x64\Debug


for /f "delims=" %%f in ('dir  /b "..\examples\*.java"') do (
    echo %%f
 %JAVA_HOME%\bin\javac  -cp %CP%  ..\examples\%%f

 )

::%JAVA_HOME%\bin\javac  -cp %CP%  ..\examples\*.java
::%JAVA_HOME%\bin\javac -verbose -cp %CP%  ..\examples\ExampleArray2.java
if %ERRORLEVEL% neq 0 ( EXIT /B -1 )
:: Run examples


cd ..
