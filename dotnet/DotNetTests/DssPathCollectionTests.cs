using Hec.Dss;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;

namespace DSSUnitTests
{

  [TestClass]
  public class DssPathCollectionTests
  {

    [TestMethod]
    public void RegularDssPathCollection()
    {
      string dssFile = @".\tempdssfile.dss";
      string srcFile = TestUtility.BasePath + @"benchmarks6/BaldEDmbrk7.dss";
      File.Delete(dssFile);
      CopyAsReadWrite(srcFile, dssFile);
      string path = @"/BALD EAGLE LOC HAV/105178.6/FLOW-CUM/17FEB1999-23FEB1999/1MIN/DAMBRKSIMBRCH/";
      using (DssReader dss = new DssReader(dssFile))
      {
        DssPathCollection paths = dss.GetCatalog(true);
        var dsspath = paths.FindExactPath(path);
        var ts = dss.GetEmptyTimeSeries(dsspath);
        Assert.IsTrue(ts.Units == "ACRE-FT");
        Assert.IsTrue(ts.DataType == "INST-CUM");
      }
    }

    public static void CopyAsReadWrite(string fromFN, string toFN)
    {
      // Set permissions to allow us to delete it
      if (File.Exists(toFN))
      {
        System.IO.FileInfo fi = new System.IO.FileInfo(toFN);
        fi.IsReadOnly = false;
        File.Delete(toFN);
      }

      File.Copy(fromFN, toFN);

      // Reset the read/write permissions
      System.IO.FileInfo fileInfo = new System.IO.FileInfo(toFN);
      fileInfo.IsReadOnly = false;
    }
  }
}
