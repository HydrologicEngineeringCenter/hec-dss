

Heclib Code


There are 13,000 lines of C test code, and 16,000 lines of FORTRAN test code.  HecLib Itself is 120,000 lines of code-- a mixture of Fortran (1/3) and C (2/3).



** Naming conventions

Fortran include files: the second to the last letter indicates the data type.  Old Fortran had issues with different data types in a common block, so we put all the integers in "i" (zdssiz), logicals in "l", "numbers" in "n", catalog stuff in "ca", array buffers in "bf", etc.  Drifted on that over time.


Function names that begin with z.

