
Get-ChildItem -Include *.cs, *.f, *.h, *.c, *.h, .cpp* -Recurse | Get-Content | Measure-Object -line 
