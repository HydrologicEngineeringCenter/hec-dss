using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;
using Hec.Dss;
using System.Linq;

namespace DSSUnitTests
{
  [TestClass]
  public class PathTesting
  {

    [TestMethod]
    public void TestIsTimeSeries()
    {
      // work through all DSS files 
      // if the file is DSS 7:
      //     get catalog that includs record type
      //     test the DssPath.IsTimeSeries() method

      var files = TestUtility.GetAllTestDssFiles();

      foreach (var file in files)
      {
        try
        {
          var fileCopy = TestUtility.GetCopyForTesting(file);
          using (DssReader dss = new DssReader(fileCopy))
          {
            var version = dss.GetDSSFileVersion();
            if (version == 7)
            {
              var catalog = dss.GetCatalog();
              foreach (var path in catalog)
              {
                var type = path.RecordType;

                var pathRaw = new DssPath(path.FullPath);

                if (type == RecordType.RegularTimeSeries
                  || type == RecordType.IrregularTimeSeries
                  || type == RecordType.RegularTimeSeriesProfile)
                {
                  Assert.IsTrue(pathRaw.IsTimeSeries());
                  Assert.IsTrue(path.IsTimeSeries());
                }
                else
                {
                  Assert.IsFalse(pathRaw.IsTimeSeries());
                  Assert.IsFalse(path.IsTimeSeries());
                }
              }

            }

          }
          File.Delete(fileCopy);  
        }catch
        { // skip DSS6 files
          Console.WriteLine("skipping "+ file);
        }
      }
    }

        [TestMethod]
    public void TestPartEVariants()
    {
      var c = new DssPath.DatelessComparer();
      Assert.IsTrue(c.Equals(P("/CHARITON RIVER/PRAIRIE HILL, MO/FLOW//15MIN/USGS/")
                , P("/CHARITON RIVER/PRAIRIE HILL, MO/FLOW//15Minute/USGS/")));

      Assert.IsTrue(c.Equals(P("/CHARITON RIVER/PRAIRIE HILL, MO/FLOW//15MINute/USGS/")
               , P("/CHARITON RIVER/PRAIRIE HILL, MO/FLOW//15MIN/USGS/")));

      Assert.IsTrue(c.Equals(P("/CHARITON RIVER/PRAIRIE HILL, MO/FLOW//1MIN/USGS/")
                , P("/CHARITON RIVER/PRAIRIE HILL, MO/FLOW//1Minute/USGS/")));



      Assert.IsFalse(c.Equals(P("/CHARITON RIVER/PRAIRIE HILL, MO/FLOW//15MINa/USGS/")
                , P("/CHARITON RIVER/PRAIRIE HILL, MO/FLOW//15Minute/USGS/")));

      Assert.IsTrue(c.Equals(P("/A/B/C//1Second/F/")
                , P("/A/B/C//1Second/f/")));


      Assert.IsFalse(c.Equals(P("/CHARITON RIVER/PRAIRIE HILL, MO/FLOW//rescue/USGS/")
                , P("/CHARITON RIVER/PRAIRIE HILL, MO/FLOW//15Minute/USGS/")));

    }

    private static DssPath P (string path)
    {
     return new DssPath(path);
    }

    [TestMethod]
    public void FilterTest()
    {
      string fn = TestUtility.GetCopyForTesting("NDFD_Wind7.dss");
      string pathname = "/SHG/CONUS/WIND DIRECTION///NDFD (YUBZ98) (001-003)/";

      /* Catalog.Paths:
          [0]: "/SHG/CONUS/WIND DIRECTION/08SEP2018:2400-11SEP2018:2400//NDFD (YUBZ98) (001-003)/"
          [1]: "/SHG/CONUS/WIND SPEED/08SEP2018:2400-11SEP2018:2400//NDFD (YWUZ98) (001-003)/"
       */

      using (var dssr = new DssReader(fn))
      {
        var targetDSSPath = new DssPath(pathname);

        // Should cache catalogue in local var?
        var cat = dssr.GetCatalog();

        // var paths = cat.Paths.Select(dssPath => dssPath.FullPath).ToArray();
        // var filterPaths = PathAssist.FilterByPart(paths, targetDSSPath);

        // No 'FilterByPart' overload that takes DSSPath objects (not strings) and accepts a dss path to filter on
        // D and E part are empty, but filtering is failing. Is there a wildcard?
        var filterPaths = PathAssist.FilterByPart(cat.Paths, targetDSSPath.Apart, targetDSSPath.Bpart, targetDSSPath.Cpart, targetDSSPath.Dpart, targetDSSPath.Epart, targetDSSPath.Fpart);
        if (filterPaths.Count == 0)
        {
          Console.WriteLine("No filtered paths found.");
          return;
        }
      }
    }

    [TestMethod]
    public void NoTrailingOrLeadingSlash()
    {
      string path = "a/b/c/d/e/f";
      var p = new DssPath(path);
      Assert.IsTrue(DssPath.IsValid(p.FullPath));
    }

    [TestMethod]
    public void isValidTest()
    {
      string path1 = "a/b/c/d/e/f";
      string path2 = "/a/b/c/d/e/f/";
      Assert.IsFalse(DssPath.IsValid(path1));
      Assert.IsTrue(DssPath.IsValid(path2));
    }

  }
}
