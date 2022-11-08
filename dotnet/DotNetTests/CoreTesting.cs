using Hec.Dss;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;
using Hec.Dss.Native;
using System.Buffers;
using System.Collections.Generic;
using System.Text;

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

      int count = DssNative.hec_dss_record_count(dss);

      int pathBufferSize = DssNative.hec_dss_CONSTANT_MAX_PATH_SIZE();
      byte[] rawCatalog = ArrayPool<byte>.Shared.Rent(count * pathBufferSize);
      
      int[] recordTypes = new int[count];
      List<string> pathNameList = new List<string>();
      try
      {
        var numRecords = DssNative.hec_dss_catalog(dss, rawCatalog, recordTypes, count, pathBufferSize);
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
        ArrayPool<byte>.Shared.Return(rawCatalog);
      }
      
      Assert.IsTrue(pathNameList.Count == 595);
      int num = 1877;
      for (int i = 0; i < 5; i++)
      {
        Assert.AreEqual("//SACRAMENTO/PRECIP-INC/01Jan" + (num + i).ToString() + "/1Day/OBS/", pathNameList[i]);
      }
      DssNative.hec_dss_close(dss);
    }
    [TestMethod]
    public void TestCatalogV7Continued()
    {
      long[] ifltab = new long[250];
      DSS.ZOpen(ref ifltab, TestUtility.BasePath + "sample7.dss");
      ZStructCatalogWrapper catStruct = DSS.zStructCatalogNew();
      int numberPaths = DSS.ZCatalog(ref ifltab, "/*/*/*Flow*/*/*/*/", ref catStruct, 1);
      Assert.IsTrue(numberPaths == 167);
      Assert.IsTrue(catStruct.PathNameList[0] == "/AMERICAN/FOLSOM/FLOW-RES IN/01Jan2006/1Day/OBS/");
      Assert.IsTrue(catStruct.PathNameList[1] == "/AMERICAN/FOLSOM/FLOW-RES OUT/01Jan2006/1Day/OBS/");
      Assert.IsTrue(catStruct.PathNameList[2] == "/EF RUSSIAN/COYOTE/FLOW-RES IN/01Mar2006/1Hour/SMOOTH/");
      Assert.IsTrue(catStruct.PathNameList[3] == "/EF RUSSIAN/COYOTE/FLOW-RES OUT/01Mar2006/1Hour//");
      Assert.IsTrue(catStruct.PathNameList[4] == "/FISHKILL CREEK/BEACON NY/FREQ-FLOW///USGS/");
      DSS.ZClose(ifltab);
    }
    [TestMethod]
    public void TestCatalogV6()
    {
      long[] ifltab = new long[250];
      DSS.ZOpen(ref ifltab, TestUtility.BasePath + "sample6_ras.dss");
      ZStructCatalogWrapper catStruct = DSS.zStructCatalogNew();
      int numberPaths = DSS.ZCatalog(ref ifltab, null, ref catStruct, 1);
      Assert.IsTrue(numberPaths == 627);
      int num = 1877;
      for (int i = 0; i < 5; i++)
      {
        Assert.IsTrue(catStruct.PathNameList[i] == "//SACRAMENTO/PRECIP-INC/01JAN" + (num + i).ToString() + "/1DAY/OBS/");
      }
      DSS.ZClose(ifltab);
    }
    [TestMethod]
    public void TestCatalogV6Continued()
    {
      long[] ifltab = new long[250];
      DSS.ZOpen(ref ifltab, TestUtility.BasePath + "sample6_ras.dss");
      ZStructCatalogWrapper catStruct = DSS.zStructCatalogNew();
      int numberPaths = DSS.ZCatalog(ref ifltab, "/*/*/*Flow*/*/*/*/", ref catStruct, 1);
      Assert.IsTrue(numberPaths == 172);
      Assert.IsTrue(catStruct.PathNameList[0] == "/AMERICAN/FOLSOM/FLOW-RES IN/01JAN2006/1DAY/OBS/");
      Assert.IsTrue(catStruct.PathNameList[1] == "/AMERICAN/FOLSOM/FLOW-RES OUT/01JAN2006/1DAY/OBS/");
      Assert.IsTrue(catStruct.PathNameList[2] == "/EF RUSSIAN/COYOTE/FLOW-RES IN/01MAR2006/1HOUR/SMOOTH/");
      Assert.IsTrue(catStruct.PathNameList[3] == "/EF RUSSIAN/COYOTE/FLOW-RES OUT/01MAR2006/1HOUR//");
      Assert.IsTrue(catStruct.PathNameList[4] == "/FISHKILL CREEK/BEACON NY/FREQ-FLOW///USGS/");
      DSS.ZClose(ifltab);
    }
    [TestMethod]
    public void TestCatalogPairedDataPathNamesV7()
    {
      long[] ifltab = new long[250];
      DSS.ZOpen(ref ifltab, TestUtility.BasePath + "sample7.dss");
      ZStructCatalogWrapper catStruct = DSS.zStructCatalogNew();
      catStruct.TypeWantedStart = 200; //paired data regular
      catStruct.TypeWantedEnd = 205;//paired data double
      int numberPaths = DSS.ZCatalog(ref ifltab, null, ref catStruct, 1);
      Assert.IsTrue(numberPaths == 595);
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
