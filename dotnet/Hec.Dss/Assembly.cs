using System;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;

namespace Hec.Dss
{
  /// <summary>
  /// This class needs to be statically invoked to find Hec.Dss.dll in the correct location.
  /// It needs to be called before any invocation of the C++ code.
  /// 
  /// We're trying to select the correct 32/64-bit dependency at runtime.
  /// https://stackoverflow.com/questions/1373100/how-to-add-folder-to-assembly-search-path-at-runtime-in-net
  /// </summary>
  
  
  public static class Assembly
  {
    [DllImport("kernel32.dll")]
    private static extern IntPtr LoadLibrary(string dllToLoad);

    static Assembly()
    {
      const string HecDss = "hecdss.dll";
      string cur = Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location);
      if (Environment.Is64BitProcess)
      {
        LoadLibrary(Path.Combine(cur, "bin64", HecDss));
      }
      else
      {
        LoadLibrary(Path.Combine(cur, "bin32", HecDss));
      }
    }
    static System.Reflection.Assembly AsmResolve(object sender, ResolveEventArgs args)
    {
      const string HecDss = "hecdss.dll";
      string cur = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location);
      if (Environment.Is64BitProcess)
      {
        return LoadAsm(System.IO.Path.Combine(cur, "bin64", HecDss));
      }
      else
      {
        return LoadAsm(System.IO.Path.Combine(cur, "bin32", HecDss));
      }
    }

    private static System.Reflection.Assembly LoadAsm(string p)
    {
      // Not sure what happens if this is already loaded in the assembly....
      try
      {
        if (File.Exists(p))
          return System.Reflection.Assembly.LoadFile(p);
        else
          return null;
      }
      catch
      {
        return null;
      }
    }

    public static void Initialize()
    {
      // This does nothing except trigger the static constructor, 
      // so we guarantee it executes exactly once.
    }


  }
}
