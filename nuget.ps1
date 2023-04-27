
copy-item  heclib\hecdss\x64\Release nuget\content  -Recurse -Force
copy-item  heclib\hecdss\x64\Release nuget\contentFiles\any\net6.0  -Recurse -Force
copy-item  heclib\hecdss\x64\Release nuget\contentFiles\any\net472  -Recurse -Force
nuget pack nuget\hecdss.nuspec