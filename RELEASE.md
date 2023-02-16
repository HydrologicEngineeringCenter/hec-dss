# Version 7-IQ-6

- Added release workflow to build assests
- Workflow `release.yml` builds ubuntu and macos assests

# Version 7-IP

- Fixed bug seen on Windows/Fortran: forrtl: severe (408): fort: (12): Variable CLINE has substring ending point 5 which is greater than the variable length of 4
- Fixed java crash on JNI call to Hec_zinquire, Hec_zlastWriteTime, and Hec_zlastWriteTimeFile
- Dropped support for older Microsoft compiler: if _MSC_VER < 1400
- Added support for macOS
