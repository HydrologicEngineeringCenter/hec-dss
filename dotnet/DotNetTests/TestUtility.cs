using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DSSUnitTests
{
  public class TestUtility
  {

    //public const string BasePath = @"M:\_Projects\HEC-DSS\test-data\";
    public const string BasePath = @"../../../../../../dss-test-data/";

    public static string GetSimpleTempFileName(string extension)
    {

      string path = Path.GetTempPath();

      var part1 = Guid.NewGuid().ToString("N").Substring(0, 8).ToLower();
      var fn = Path.Combine(path, part1 + extension);

      if (File.Exists(fn))
        throw new Exception("File allready exists");

      File.Create(fn).Close();

      return fn;
    }
  }
}
