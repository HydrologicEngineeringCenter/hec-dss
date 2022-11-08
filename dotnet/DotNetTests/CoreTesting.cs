using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;
using Hec.Dss.Native;

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
      long[] ifltab = new long[250];
      DSS.ZOpen(ref ifltab, TestUtility.BasePath + "sample7.dss");
      ZStructCatalogWrapper catStruct = DSS.zStructCatalogNew();
      int numberPaths = DSS.ZCatalog(ref ifltab, null, ref catStruct, 1);
      Assert.IsTrue(numberPaths == 595);
      int num = 1877;
      for (int i = 0; i < 5; i++)
      {
        Assert.IsTrue(catStruct.PathNameList[i] == "//SACRAMENTO/PRECIP-INC/01Jan" + (num + i).ToString() + "/1Day/OBS/");
      }
      DSS.ZClose(ifltab);
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
        Assert.IsTrue(catStruct.PathNameList[i] == "//SACRAMENTO/PRECIP-INC/01Jan" + (num + i).ToString() + "/1Day/OBS/");
      }
      DSS.ZClose(ifltab);
    }

    [TestMethod]
    public void TestTimeSeries1()
    {
      long[] ifltab = new long[250];
      ZStructTimeSeriesWrapper tss1,tss2, tss3, tss4;
      float[] fvalues = new float[200];
      double[] dvalues = new double[300];
      int[] itimes = new int[300];
      string cdate = new string('1', 13);
      string ctime = new string('1', 10);
      int valueTime,status;
      DSS.ZOpen(ref ifltab, "dotnet_example7.dss");
      for (int i = 0; i < 200; i++)
      {
        fvalues[i] = (float)i;
      }
      tss1 = DSS.ZStructTsNewRegFloats("/Basin/Location/Flow//1Hour/C Test/",ref fvalues, 200, "21Jan2001", "1200", "cfs", "Inst-Val");
      Assert.IsTrue(tss1.StartJulianDate == 36911);
      Assert.IsTrue(tss1.StartTimeSeconds == 43200);
      status = DSS.ZTsStore(ref ifltab, ref tss1, 0);
     // ZStructTimeSeriesWrapper tss;
      Assert.IsTrue(status == 0);
      for (int i = 0; i < 300; i++)
      {
        dvalues[i] = (double)i;
        itimes[i] = i * 1440;
      }
      tss2 = DSS.ZStructTsNewIrregDoubles("/Basin/Location/Flow//~1Day/C Example/",ref dvalues, 300,ref itimes, 60, "20April2012", "cfs", "Inst-Val");
      Assert.IsTrue( Hec.Dss.Time.IsUndefinedTime(tss2.StartJulianDate));
      Assert.IsTrue(tss2.StartTimeSeconds == -1);
      status = DSS.ZTsStore(ref ifltab, ref tss2, 0);
      Assert.IsTrue(status == 0);
      tss3 = DSS.ZStructTsNew("/Basin/Location/Flow/01Jan2001/1Hour/C Test/");
      Assert.IsTrue(Hec.Dss.Time.IsUndefinedTime(tss3.StartJulianDate));
      Assert.IsTrue(tss3.StartTimeSeconds == -1);
      status = DSS.ZTsRetrieve(ref ifltab, ref tss3, -1, 1, 0);
      Assert.IsTrue(status == 0);
      valueTime = tss3.StartTimeSeconds;
      Assert.IsTrue(valueTime == 43200);
      Assert.IsTrue(tss3.NumberValues == 200);
      for (int i = 0; i < tss3.NumberValues; i++)
      {
        DSS.GetDateAndTime(valueTime, 1, tss3.StartJulianDate, ref cdate, 13,ref ctime, 10);
        Assert.IsTrue(tss3.FloatValues[i] == (double)i);
        if (i == 0)
        {
          Assert.IsTrue(cdate == "21Jan2001");
          Assert.IsTrue(ctime == "12:00:00");
        }
        else if (i == 8)
        {
          Assert.IsTrue(cdate == "21Jan2001");
          Assert.IsTrue(ctime == "20:00:00");
        }
        else if (i == 136)
        {
          Assert.IsTrue(cdate == "27Jan2001");
          Assert.IsTrue(ctime == "04:00:00");
        }
        valueTime += 3600;
      }
      tss4 = DSS.ZStructTsNewTimes("/Basin/Location/Flow//~1Day/C Example/", "19April2012", "2400", "01July2013", "2400");
      status = DSS.ZTsRetrieve(ref ifltab,ref tss4, 0, 2, 0);
      Assert.IsTrue(status == 0);
      for (int i = 0; i < tss4.NumberValues; i++)
      {
        DSS.GetDateAndTime(tss4.Times[i], tss4.TimeGranularitySeconds, tss4.JulianBaseDate, ref cdate, 13, ref ctime, 10);
        Assert.IsTrue(tss4.DoubleValues[i] == (double)i);
        Assert.IsTrue(ctime == "2400");
        if (i == 0)
          Assert.IsTrue(cdate == "19Apr2012");

        else if (i == 8)
          Assert.IsTrue(cdate == "27Apr2012");

        else if (i == 153)
          Assert.IsTrue(cdate == "19Sep2012");

      }
      DSS.ZClose(ifltab);
    }
    [TestMethod]
    public void TestTimeSeries2()
    {
      long[] ifltab = new long[250];
      ZStructTimeSeriesWrapper tss1, tss2;
      float[] fvalues = new float[200];
      int[] quality = new int[200];
      sbyte[] cnotes1 = new sbyte[10000];
      int noteCount;
      string cdate = new string('1', 13);
      string ctime = new string('1', 10);
      int valueTime, status, n;
      int ich;
      status = DSS.ZOpen(ref ifltab, TestUtility.BasePath + "dotnet_example8.dss");
      Assert.IsTrue(status == 0);
      tss2 = DSS.ZStructTsNew("/Basin/Location/Flow/01JAN2010/1Hour/Java Example/");
      tss2.NumberValues = 200;
      tss2.StartJulianDate = 40197;
      tss2.StartTimeSeconds = 86400;
      tss2.EndJulianDate = 40206;
      tss2.EndTimeSeconds = 25200;
      tss2.TimeGranularitySeconds = 60;
      tss2.TimeIntervalSeconds = 3600;
      ich = 66;
      noteCount = 0;
      for (int i = 0; i < 200; i++)
      {
        fvalues[i] = (float)i;
        quality[i] = i + 10;
        n = i / 25;
        n = i - (n * 25) + 1;
        for (int j = 0; j < n; j++)
        {
          cnotes1[noteCount++] = Convert.ToSByte(ich + j - 1 + 32);
        }
        cnotes1[noteCount++] = Convert.ToSByte('\0');
      }
      tss1 = DSS.ZStructTsNewRegFloats("/Basin/Location/Flow//1Hour/C with Quality and C notes/", ref fvalues, 200, "21Jan2001", "1200", "cfs", "Inst-Val");
      tss1.Quality = quality;
      tss1.QualityElementSize = 1;
      tss1.CNotes = cnotes1;
      tss1.CNotesLengthTotal = noteCount;
      status = DSS.ZTsStore(ref ifltab, ref tss1, 0);
      System.Threading.Thread.Sleep(100);
      Assert.IsTrue(status == 0);
      tss2 = DSS.ZStructTsNew("/Basin/Location/Flow/01Jan2001/1Hour/C with Quality and C notes/");
      status = DSS.ZTsRetrieve(ref ifltab, ref tss2, -1, 1, 1);
      System.Threading.Thread.Sleep(100);
      Assert.IsTrue(status == 0);
      valueTime = tss2.StartTimeSeconds;
      noteCount = 0;
      for (int i = 0; i < tss2.NumberValues;i++)
      {
        DSS.GetDateAndTime(valueTime, 1, tss2.StartJulianDate, ref cdate, 13, ref ctime, 10);
        Assert.IsTrue(tss2.FloatValues[i] == (float)i);
        Assert.IsTrue(tss2.Quality[i] == 10+i);
        if (i == 0)
        {
          Assert.IsTrue(cdate == "21Jan2001");
          Assert.IsTrue(ctime == "12:00:00");
          Assert.IsTrue(tss2.CNotes[noteCount] == 'a');
        }
        noteCount += tss2.CNotes.Length+1;
        valueTime += 3600;
      }
      DSS.ZClose(ifltab);
    }
    [TestMethod]
    public void TestRetrieveGrid6()
    {
      long[] ifltab = new long[250];
      int status;
      ////ZStructGridWrapper gs;
      status = DSS.ZOpen(ref ifltab, "containsGrids.dss");
      //if (status != 0)
      //  Assert.Fail();
      ////gs = DSS.ZStructGridNew(@"/SHG/LAKE WINNEBAGO/PRECIP/01JUN2016:0600/01JUN2016:1200/WPC-QPF/");
      ////status = DSS.ZgRetrieve(ref ifltab, ref gs);
      //if (status != 0)
      //  Assert.Fail();
      //DSS.ZClose(ifltab);

    }

  }
}
