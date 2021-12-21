
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


It has been reported the Centos binaries worked in ubunto,after installing the follwing:

zlib, math, quadmath, and gfortran for pydsstools to work in Ubuntu.


To build heclib, cd heclib/heclib
>make 


Windows
>build_release.bat 

===========
The primary user interface for DSS files is HEC-DssVue.
https://www.hec.usace.army.mil/confluence/dssvuedocs/latest


