# heclib-test
DSS is tested with Java, C, and FORTRAN code. Java testing is not discused here.


.\C   		-- standalone C test/sample programs
.\Dss-C         -- C code that tests many features of DSS
.\Fortran       -- code that tests DSS from the FORTRAN language

# Input Data Setup required for the tests
dss-test-data needs to be checked out inside the hec-dss repo.  Some of the testing requireds these files.

https://github.com/HydrologicEngineeringCenter/dss-test-data


# How can you run the C tests.
The C tests are several standalone C programs that are run with a batch file or unix script.

```
# Linux
cd hec-dss/test/C
./unix_test
```
# Windows 
```
:: assumes Visual Stuido 2019 and intel fortran are installed 
cd hec-dss/test
test_c

```

# How can you run the Dss-C test program
```
# Linux
cd hec-dss/test/Dss-c
make test
```


# How can you run the Fortran tests?
```
# Linux
cd hec-dss/test/Fortran
make test
```

