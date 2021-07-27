using System;
using Hec.Dss;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;

namespace DSSUnitTests
{
  [TestClass]
  public class LocationInfoTesting
  {

    [TestMethod]
    public void ReadLocationFromVersion6File()
    {
      using (var r = new DssReader(TestUtility.BasePath + "sample.dss"))
      {
        string path = "//SACRAMENTO/TEMP-MAX/01JAN1877/1DAY/OBS/";
        var L = r.GetLocationInfo(path);

      }
    }

    /// <summary>
    /// how much difference in space to store a string in supplemnetal info
    /// 1000 series  with 1200 points each
    /// </summary>
    [TestMethod]
    public void WhatIsCostOfStringinSupplemental()
    {
      String fn = "WhatIsCostOfStringinSupplemental.dss";
      File.Delete(fn);
      using (var w = new DssWriter(fn))
      {
        for (int i = 1; i < 1000; i++)
        {
          var ts = TimeSeriesTest.CreateSampleTimeSeries(DateTime.Now.Date, "cfs", "Per-AVE", size: 1200);
          ts.Path = new DssPath("/A/" + i.ToString().PadLeft(4, 'B')+"/flow//1Day/ver1/");
          LocationInformation loc = new LocationInformation();
          loc.Supplemental =  "WAT_tag:Realization/" + i +";";
          ts.LocationInformation = loc;
          w.StoreLocation(ts.Path.FullPath, loc, true);
          w.Write(ts);
        }
        
      }
    }
    [TestMethod]
    public void SupplementalInfo()
    {
      using (var r = new DssReader(TestUtility.BasePath + "VerticalDatumTest_v7.dss"))
      {

        var P = r.GetCatalog();
        for (int i = 0; i < P.Count; i++)
        {
          if (P[i].Cpart.ToLower().StartsWith("location"))
          {
            Console.WriteLine(P[i].FullPath);
          }
        }

        var L = r.GetLocationInfo("//KEYS/Location Info////");

        Console.WriteLine(L.Supplemental);
        //verticalDatumInfo:H4sIAAAAAAAAAHXQwQrCMAwG4PueouzedQ4mE2phIHjz6D1oCoWuhTXt89taUNR5C/8fvkBkwpXMDSy/A8WFG6c9i87QsdXUqoYx6YBMwtqry/l64sNBio+0rKHFlDPv1H43doMU76DUXuuAxDCQWYAw82ADPi/klvy3/0rqQgIbUfVdL0Udiykquu3TGjf4OfPT9JcfN/kc/n5JNQ84o4x9PAEAAA==;
        /**
         * 
         <vertical-datum-info unit="ft">
          <native-datum>NGVD-29</native-datum>
          <elevation>615.2</elevation>
          <offset estimate="false">
            <to-datum>NGVD-29</to-datum>
            <value>0.0</value>
          </offset>
          <offset estimate="true">
            <to-datum>NAVD-88</to-datum>
            <value>0.5</value>
          </offset>
        </vertical-datum-info>

         * 
         **/
        var vdi = L.VerticalDatumInfo;
        var xml = vdi.ToXml(true);
        Console.WriteLine(xml);
        DoAssertions(CreateVDI(), vdi);
      }
    }

    [TestMethod]
    public void WriteThenReadVerticalDatum()
    {
      VerticalDatumInfo vdi = CreateVDI();

      var locationInfo = new LocationInformation();
      LocationInformation loc;
      locationInfo.XOrdinate = 765.432;
      locationInfo.VerticalDatumInfo = vdi;
      string path = "a/b/Location Info";
      var dssFile = "WriteThenReadVerticalDatum.dss";
      File.Delete(dssFile);
      using (var w = new DssWriter(dssFile))
      {
        w.StoreLocation(path, locationInfo);
      }

      using (var r = new DssReader(dssFile))
      {
        loc = r.GetLocationInfo(path);
        DoAssertions(CreateVDI(), loc.VerticalDatumInfo);
        Assert.AreEqual(locationInfo.XOrdinate, loc.XOrdinate);
      }

      Console.WriteLine(loc.VerticalDatumInfo.ToXml(true));
    }

    private static VerticalDatumInfo CreateVDI()
    {
      VerticalDatumInfo vdi = new VerticalDatumInfo();

      vdi.NativeDatum = "NGVD-29";
      vdi.Units = "ft";
      vdi.NGVD29OffsetIsEstimate = false;
      vdi.NGVD29Offset = 0.0;
      vdi.NAVD88Offset = 0.5;
      vdi.NAVD88OffsetIsEstimate = true;
      vdi.Elevation = 615.2;
      return vdi;
    }

    private static void DoAssertions(VerticalDatumInfo expected, VerticalDatumInfo vdi)
    {
      Assert.AreEqual(expected.Elevation, vdi.Elevation, 0.01);

      Assert.AreEqual(expected.NativeDatum, vdi.NativeDatum);
      Assert.AreEqual(expected.Units, vdi.Units);
      Assert.AreEqual(expected.NGVD29OffsetIsEstimate, vdi.NGVD29OffsetIsEstimate);
      Assert.AreEqual(expected.NGVD29Offset, vdi.NGVD29Offset);
      Assert.AreEqual(expected.NAVD88OffsetIsEstimate, vdi.NAVD88OffsetIsEstimate);
      Assert.AreEqual(expected.NAVD88Offset, vdi.NAVD88Offset);
    }
  }
}
