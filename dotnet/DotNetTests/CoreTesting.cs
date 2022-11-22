using Hec.Dss;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;

namespace DSSUnitTests
{
  [TestClass]
  public class CoreTesting
  {
  

    [TestMethod]
    public void TestOpen()
    {
      long[] ifltab = new long[250];
      int status;
            string fn = TestUtility.GetSimpleTempFileName(".dss");
      File.Delete(fn);
         IntPtr dss;
      status = DssNative.hec_dss_open(fn,out dss);

      if (status != 0)
        Assert.Fail();
      if (!File.Exists(fn))
        Assert.Fail();
       DssNative.hec_dss_close(dss);

    }
    [TestMethod]
    public void TestCatalogV7()
    {
      IntPtr dss;
      var status = DssNative.hec_dss_open(TestUtility.BasePath + "sample7.dss", out dss);

      var pathNameList = DssReader.GetRawCatalog(dss, out int[] recordTypes);

      /*      int count = DssNative.hec_dss_record_count(dss);

            int pathBufferSize = DssNative.hec_dss_CONSTANT_MAX_PATH_SIZE();

            //byte[] rawCatalog = ArrayPool<byte>.Shared.Rent(count * pathBufferSize);
            //var mem = rawCatalog.AsMemory();
            //MemoryHandle handle = mem.Pin();

            byte[] rawCatalog = new byte[count*pathBufferSize];
            int[] recordTypes = new int[count];

            List<string> pathNameList = new List<string>(count);
            byte[] filter = new byte[] {0};
            try //
            {
              var numRecords = DssNative.hec_dss_catalog(dss, rawCatalog, recordTypes, new byte[] { 0 }, count, pathBufferSize);
              for (int i = 0; i < numRecords; i++)
              {
                int start = i * pathBufferSize;
                var end = System.Array.IndexOf(rawCatalog, (byte)0,start); // get rid of trailing \0\0
                int size = Math.Min(end - start, pathBufferSize);
                pathNameList.Add(Encoding.ASCII.GetString(rawCatalog, start, size));
              }
            }
            finally
            {
            //  handle.Dispose();
              //ArrayPool<byte>.Shared.Return(rawCatalog);
            }
            */
      Assert.AreEqual(595, pathNameList.Count);
      int num = 1877;
      for (int i = 0; i < 5; i++)
      {
        string p = "//SACRAMENTO/PRECIP-INC/01Jan" + (num + i).ToString() + "/1Day/OBS/";
        var index = pathNameList.IndexOf(p);
        Assert.IsTrue((index >= 0), "path not found:'" + p + "'");
      }
      DssNative.hec_dss_close(dss);
    }
    [TestMethod]
    public void TestCatalogV7Continued()
    {
      var status = DssNative.hec_dss_open(TestUtility.BasePath + "sample7.dss", out IntPtr dss);

      var catalog = DssReader.GetRawCatalog(dss,out int[] recordTypes,"/*/*/*Flow*/*/*/*/");

      DssNative.hec_dss_close(dss);
      Assert.AreEqual(167,catalog.Count);
      
      Assert.IsTrue(catalog.Contains("/AMERICAN/FOLSOM/FLOW-RES IN/01Jan2006/1Day/OBS/"));
      Assert.IsTrue(catalog.Contains("/AMERICAN/FOLSOM/FLOW-RES OUT/01Jan2006/1Day/OBS/"));
      Assert.IsTrue(catalog.Contains("/EF RUSSIAN/COYOTE/FLOW-RES IN/01Mar2006/1Hour/SMOOTH/"));
      Assert.IsTrue(catalog.Contains("/EF RUSSIAN/COYOTE/FLOW-RES OUT/01Mar2006/1Hour//"));
      Assert.IsTrue(catalog.Contains("/FISHKILL CREEK/BEACON NY/FREQ-FLOW///USGS/"));
    }

  }
}
