
This is the HEC-DSS library source tree.  DSS is a database optimized for timeseries data.  


Documentation:  https://www.hec.usace.army.mil/software/hec-dss/documentation.aspx



Windows Dependencies

heclib.dll 
    libifcoremdd.dll
    libifportMD.dll
    libmmdd.dll
    KERNEL32.dll
    VCRUNTIME140D.dll
    ucrtbased.dll



# RedHat/RockyLinux -- Install Build Dependencies

yum install -y \
     git \
     gcc \
     gcc-gfortran \
     make \
     zlib-devel \
     java-devel \
	 gcc-c++

export JAVA_HOME=/etc/alternatives/java_sdk_11_openjdk


# Ubuntu -- Install Build Dependencies

```
sudo apt-get install build-essential
sudo apt-get install gfortran
sudo apt-get install zlib1g-dev
apt-get install default-jdk
```

It has been reported the Centos binaries worked in ubunto,after installing the following:

zlib, math, quadmath, and gfortran for pydsstools to work in Ubuntu.


## To build and test heclib

### Linux 
```
git clone https://github.com/HydrologicEngineeringCenter/hec-dss.git
cd hec-dss
git clone https://github.com/HydrologicEngineeringCenter/dss-test-data.git
In the hec-dss/heclib directory, execute make clean ; make
In the hec-dss/test/C directory, execute ./unix_test
In the hec-dss/test/Dss-C directory, execute make clean ; make test
In the hec-dss/test/Fortran directory, execute make clean ; make test

```

### Solaris sparc sun4v  

developerstudio12.6 
#pkg contents system/library/fortran-runtime
export PATH=$PATH:/opt/developerstudio12.6/bin:/usr/gcc/7/bin
export JAVA_HOME=/usr/jdk/instances/jdk1.8.0
export LD_LIBRARY_PATH=/usr/gcc/7/lib/sparcv9

```
 bash
 git clone https://github.com/HydrologicEngineeringCenter/hec-dss.git
 cd hec-dss
 git clone https://github.com/HydrologicEngineeringCenter/dss-test-data.git
 In the hec-dss/heclib directory, execute gmake clean ; gmake
 In the hec-dss/test/C directory, execute ./unix_test
 In the hec-dss/test/Dss-C directory, execute gmake clean ; gmake test
 In the hec-dss/test/Fortran directory, execute gmake -f Makefile.Solaris clean ; gmake -f Makefile.Solaris test

```

### Windows

For Windows we are using the intel compiler and Visual Studio 2022.


The environment variables IFORT_COMPILER_LIB and JAVA_HOME need to be set.
example: 

```cmd
set IFORT_COMPILER_LIB=C:\Program Files (x86)\Intel\oneAPI\compiler\2024.2\lib
set JAVA_HOME=c:\bin\jdk1.8.0

git clone https://github.com/HydrologicEngineeringCenter/hec-dss.git
cd hec-dss
git clone https://github.com/HydrologicEngineeringCenter/dss-test-data.git

build_release.bat 

cd test
test_c.bat
cd ..
test_Dss-C.bat
test_fortran.bat


```

## running the java tests

```bash

./gradlew test --tests hec.heclib.* --info
# check test results here;
# hec-monolith/hec-monolith-compat/build/reports/tests/test

```




## User Interface
The primary user interface for DSS files is HEC-DSSVue.
https://www.hec.usace.army.mil/confluence/dssdocs/dssvueum/


