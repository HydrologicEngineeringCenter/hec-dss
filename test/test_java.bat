cd %~dp0
cd ..

::call test_setup
:: compile and run some java programs

set JAVA_HOME="C:\Programs\Java\jdk1.8.0"
set PLATFORM=x64

set SRC=test/Java/src/test/java

set CP=jar\heclib.jar;jar\hec.jar;jar\hecData.jar;jar\rma.jar;jar\junit-platform-console-standalone-1.3.1.jar;jar\hec-dssvue-v3.0.1.jar;hecClientServer-v1.1-dev.jar
set CP=%CP%;test\Java\src\test\java
set JLIB=-Djava.library.path=heclib\javaHeclib\%PLATFORM%\Release
::# compile unit tests


%JAVA_HOME%\bin\javac  -cp "%CP%"  %SRC%/hec/heclib/*.java
%JAVA_HOME%\bin\javac  -cp "%CP%"  %SRC%/hec/heclib/dss/*.java
%JAVA_HOME%\bin\javac  -cp "%CP%"  %SRC%/hec/heclib/grid/*.java
%JAVA_HOME%\bin\javac  -cp "%CP%"  %SRC%/hec/hecmath/*.java

if %ERRORLEVEL% neq 0 ( EXIT /B -1 )
 
:: Run unit tests
%JAVA_HOME%\bin\java %JLIB% -jar jar\junit-platform-console-standalone-1.3.1.jar  --class-path %CP% -n=hec.* --scan-classpath
if %ERRORLEVEL% neq 0 (
echo "failure in junit"
 EXIT /B -1 
)

cd test