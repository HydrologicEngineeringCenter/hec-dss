using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DSSUnitTests
{
  public class TestUtility
  {

    private const string BasePath = @"../../../../../../dss-test-data/";


    internal static string[] GetAllTestDssFiles()
    {
     return  Directory.GetFiles(TestUtility.BasePath, "*.dss", SearchOption.AllDirectories);
    }
    /// <summary>
    /// Gets path to a temporary file (that does not exist)
    /// </summary>
    /// <param name="extension">including the .</param>
    /// <returns></returns>
    /// <exception cref="Exception"></exception>
    internal static string GetSimpleTempFileName(string extension)
    {

      string path = Path.GetTempPath();

      var part1 = Guid.NewGuid().ToString("N").Substring(0, 8).ToLower();
      var fn = Path.Combine(path, part1 + extension);

      if (File.Exists(fn))
        throw new Exception("File allready exists");

      return fn;
    }

   // [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.Synchronized)]
    internal static string GetCopyForTesting(string fileName)
    {
      string tempFileName = GetSimpleTempFileName(".dss");
      string fn = Path.Combine(BasePath, fileName);
      Console.WriteLine("copying "+fn+"  to "+tempFileName);
      File.Copy(fn, tempFileName);

      return tempFileName;
    }
  }
}
