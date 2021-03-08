cd %~dp0

set JAVA_HOME="C:\Programs\Java\jdk1.8.0"

call test_setup :: changes to bin directory to run..

set JAR=..\..\jar
set CP=%jar%\hec.jar;%jar%\rma.jar;%jar%\junit-platform-console-standalone-1.3.1.jar;%jar%\hec-dssvue-v3.0.1.jar;%jar%\hecPlots-v1.1-dev.jar;.
::set CP=..\..\jar\*
set JLIB=-Djava.library.path=..\..\heclib\javaHeclib\x64\Debug
::# compile unit tests

%JAVA_HOME%\bin\javac  -cp "%CP%"  ../Java/src/hec/dss/testing/*.java
if %ERRORLEVEL% neq 0 ( EXIT /B -1 )
%JAVA_HOME%\bin\javac  -cp "..\Java\src\;%CP%"  ../Java/Main.java
if %ERRORLEVEL% neq 0 ( EXIT /B -1 )

 
:: Run unit tests
%JAVA_HOME%\bin\java %JLIB% -cp "..\Java\src;%CP%;..\Java"  Main
if %ERRORLEVEL% neq 0 (
echo "failure in junit"
 EXIT /B -1 
)

cd ..