:: 32-bit Windows Build for Heclib/DSS
:: we don't need 32bit javaHeclib.dll, dont' need nws_shef for 32 bit
::
call vs_env32.bat
echo on
SET DSS_PLATFORM_DIR=Win32
dotnet restore dotnet\Hec.Dss\Hec.Dss.csproj

cd heclib\heclib_f
nmake -f Makefile.win  clean all

cd %~dp0
msbuild dss.sln /p:Configuration=Release /p:Platform=Win32

cd %~dp0

