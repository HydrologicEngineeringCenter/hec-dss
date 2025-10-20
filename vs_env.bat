:: TO DO.. remove need for Intel env setup.
:: "C:\Program Files\Microsoft Visual Studio\2022\Professional\VC\Auxiliary\Build\vcvars64.bat"
if not defined ONEAPI_ROOT set "ONEAPI_ROOT=C:\Program Files (x86)\Intel\oneAPI"
call "%%ONEAPI_ROOT%%\setvars.bat" intel64 vs2022