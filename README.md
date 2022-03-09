
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



# RedHat/Centos -- Install Build Dependencies

yum install -y \
     git \
     gcc \
     gcc-gfortran \
     make \
     zlib-devel \
     java-1.8.0-openjdk-devel \
	 gcc-c++

# Ubuntu -- Install Build Dependencies

sudo apt-get install build-essential
sudo apt-get install gfortran
sudo apt-get install zlib1g-dev
apt-get install default-jdk


It has been reported the Centos binaries worked in ubunto,after installing the following:

zlib, math, quadmath, and gfortran for pydsstools to work in Ubuntu.


## To build and test heclib

### Linux 
```
1.	git clone -j2 --recurse-submodules https://github.com/HydrologicEngineeringCenter/hec-dss.git
2.	In the hec-dss/heclib directory, execute make clean ; make
3.	In the hec-dss/test/C directory, execute ./unix_test
4.	In the hec-dss/test/Dss-C directory, execute make clean ; make test
5.	In the hec-dss/test/Fortran directory, execute make clean ; make test

```

### Solaris sparc sun4v  

```
1.  bash
2.	git clone -j2 --recurse-submodules https://github.com/HydrologicEngineeringCenter/hec-dss.git
3.	In the hec-dss/heclib directory, execute gmake clean ; gmake
4.	In the hec-dss/test/C directory, execute ./unix_test
5.	In the hec-dss/test/Dss-C directory, execute gmake clean ; gmake test
6.	In the hec-dss/test/Fortran directory, execute gmake -f Makefile.Solaris clean ; gmake -f Makefile.Solaris test

```

### Windows

```cmd

build_release.bat 
```

## running the java tests

TO DO




## User Interface
The primary user interface for DSS files is HEC-DssVue.
https://www.hec.usace.army.mil/confluence/dssvuedocs/latest


