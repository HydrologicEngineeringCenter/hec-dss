cd %~dp0
call  ..\..\vs_env.bat 
devenv Dss7-Fortran.vfproj /Build "Debug|x64" /Project Dss7-Fortran

exit