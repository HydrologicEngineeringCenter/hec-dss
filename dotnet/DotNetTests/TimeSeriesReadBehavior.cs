using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using Hec.Dss;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace DSSUnitTests
{
  [TestClass]
  public class TimeSeriesReadBehavior
  {
    string filename6 = System.IO.Path.Combine(TestUtility.BasePath, "MoRiverObs_TimeWindowTest6.dss");
    string filename7 = System.IO.Path.Combine(TestUtility.BasePath, "MoRiverObs_TimeWindowTest7.dss");
    string PathNoDpart = "/CHARITON RIVER/PRAIRIE HILL, MO/FLOW//15MIN/USGS/";
    string Path_01Jan2010_Dpart = "/CHARITON RIVER/PRAIRIE HILL, MO/FLOW/01Jan2010/15MIN/USGS/";


    [TestMethod]
    public TimeSeries ReadTs(String fileName, String path, 
      DateTime t1 = default(DateTime), DateTime t2=default(DateTime))
    {
      using (DssReader r = new DssReader(fileName))
      {
        TimeSeries ts = null;
        if (t1 == default(DateTime))
          ts = r.GetTimeSeries(new DssPath(path));
        else
          ts = r.GetTimeSeries(new DssPath(path), t1, t2);
        return ts;
      }
    }

    [TestMethod]
    public void EmptyDPart()
    {
      var ts6 = ReadTs(filename6, PathNoDpart);
      Console.WriteLine("ts6.count="+ts6.Count);
      var ts7 = ReadTs(filename7, PathNoDpart);
      Console.WriteLine("ts7.count=" + ts7.Count);

      // TO DO.. is the the data the same?

    }

    [TestMethod]
    public void GiveTimeWindowEmptyDPart()
    {
      DateTime t1 = new DateTime(2009, 1, 1);
      DateTime t2 = t1.AddYears(1).AddDays(-1);

      var ts6 = ReadTs(filename6, PathNoDpart,t1,t2);
      Console.WriteLine("ts6.count=" + ts6.Count);
      var ts7 = ReadTs(filename7, PathNoDpart,t1,t2);
      Console.WriteLine("ts7.count=" + ts7.Count);

    }

    [TestMethod]
    public void GiveTimeWindowAnd_Path_01Jan2010_Dpart()
    {
      DateTime t1 = new DateTime(2009, 1, 1);
      DateTime t2 = t1.AddYears(1).AddDays(-1);

      var ts6 = ReadTs(filename6, Path_01Jan2010_Dpart, t1, t2);
      Console.WriteLine("ts6.count=" + ts6.Count);
      var ts7 = ReadTs(filename7, Path_01Jan2010_Dpart, t1, t2);
      Console.WriteLine("ts7.count=" + ts7.Count);

    }

    [TestMethod]
    public void SkipTimeWindow_Path_01Jan2010_Dpart()
    {
      var ts6 = ReadTs(filename6, Path_01Jan2010_Dpart);
      Console.WriteLine("ts6.count=" + ts6.Count);
      var ts7 = ReadTs(filename7, Path_01Jan2010_Dpart);
      Console.WriteLine("ts7.count=" + ts7.Count);

    }

    [TestMethod]
    public void TimeWindowStartBeforeBlockEmptyDPart()
    {
      DateTime t1 = new DateTime(2007, 8, 2);
      DateTime t2 = new DateTime(2007, 10, 1);

      var ts6 = ReadTs(filename6, PathNoDpart,t1,t2);
      Console.WriteLine("ts6.count=" + ts6.Count);
      var ts7 = ReadTs(filename7, PathNoDpart,t1,t2);
      Console.WriteLine("ts7.count=" + ts7.Count);

    }

    [TestMethod]
    public void TimeWindowOutsideBlockEmptyDPart()
    {
      DateTime t1 = new DateTime(2007, 8, 2);
      DateTime t2 = new DateTime(2007, 8, 10);

      var ts6 = ReadTs(filename6, PathNoDpart, t1, t2);
      Console.WriteLine("ts6.count=" + ts6.Count);
      var ts7 = ReadTs(filename7, PathNoDpart, t1, t2);
      Console.WriteLine("ts7.count=" + ts7.Count);

    }


  }
}
