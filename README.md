
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

```
1.	git pull origin master
2.  git submodule update --init --recursive   # this loads up the dss-test-data directory
3.	In the hec-dss/heclib directory, execute make clean ; make
3.	In the hec-dss/test/C directory, execute ./unix_test
4.	In the hec-dss/test/Dss-C directory, execute make clean ; make test
5.	In the hec-dss/test/Fortran directory, execute make clean ; make test

```


```cmd
Windows
build_release.bat 
```

## running the java tests

TO DO




## User Interface
The primary user interface for DSS files is HEC-DssVue.
https://www.hec.usace.army.mil/confluence/dssvuedocs/latest


