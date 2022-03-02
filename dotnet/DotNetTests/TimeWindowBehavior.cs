using System;
using System.Collections.Generic;
using System.IO;
using Hec.Dss;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace DSSUnitTests
{
  [TestClass]
  public class TimeWindowBehavior
  {
    /*
TimeWindowBehavior.StrictlyInclusive - currently how it works
TimeWindowBehavior.Cover - checks to see if an extra value before or after should be read


And another enum for how to handle spans of repeated values

ConsecutiveValueCompression.None - currently how this works
ConsecutiveValueCompression.NoData - trim out (or never read) spans of no data (keep first no data and last)
ConsecutiveValueCompression.ZeroAndNoData
ConsecutiveValueCompression.AnyValue      
     */
    [TestMethod]
    public void TimeWindowBehavior_Cover_StartShiftedRight_EndShiftedLeft()
    {
      var fn = "TimeWindowBehavior_Cover_StartShiftedRight_EndShiftedLeft.dss";
      File.Delete(fn);
      using (DssWriter dss = new DssWriter(fn))
      {
        var ts = CreateRegularTimeSeries();
        dss.Write(ts);

        DateTime t1 = ts.Times[2].AddHours(1);
        DateTime t2 = ts.Times[5].AddHours(-2);

        var ts2 = dss.GetTimeSeries(ts.Path, t1, t2, TimeWindow.TimeWindowBehavior.Cover);

        Assert.AreEqual(ts.Times[2], ts2.Times[0]);
        Assert.AreEqual(ts.Times[5], ts2.Times[ts2.Times.Length - 1]);

        Assert.AreEqual(2.0, ts2.Values[0]);
        Assert.AreEqual(5, ts2.Values[ts2.Times.Length - 1]);

      }

    }

    [TestMethod]
    public void TimeWindowBehavior_Cover_StartShiftedRight()
    {
      var fn = "TimeWindowBehavior_Cover_StartShiftedRight.dss";
      File.Delete(fn);
      using (DssWriter dss = new DssWriter(fn))
      {
        var ts = CreateRegularTimeSeries();
        dss.Write(ts);

        DateTime t1 = ts.Times[3].AddHours(13);
        DateTime t2 = ts.Times[7];

        var ts2 = dss.GetTimeSeries(ts.Path, t1, t2, TimeWindow.TimeWindowBehavior.Cover);

        Assert.AreEqual(ts.Times[3], ts2.Times[0]);
        Assert.AreEqual(ts.Times[7], ts2.Times[ts2.Times.Length - 1]);

        Assert.AreEqual(3.0, ts2.Values[0]);
        Assert.AreEqual(7.0, ts2.Values[ts2.Times.Length - 1]);

      }

        }

        /// <summary>
        ///   1/1/1985 10:30:00 AM 0
        ///   1/2/1985 10:30:00 AM 1
        ///   1/3/1985 10:30:00 AM 2
        ///   1/4/1985 10:30:00 AM 3
        ///   1/5/1985 10:30:00 AM 4
        ///   1/6/1985 10:30:00 AM 5
        ///   1/7/1985 10:30:00 AM 6
        ///   1/8/1985 10:30:00 AM 7
        ///   1/9/1985 10:30:00 AM 8
        ///   1/10/1985 10:30:00 AM 9
        ///   
        /// 1/2/1985 10:30:00 AM 1
        /// 1/3/1985 10:30:00 AM 2
        /// 1/4/1985 10:30:00 AM 3
        /// 1/5/1985 10:30:00 AM 4
        /// 1/6/1985 10:30:00 AM 5
        /// 1/7/1985 10:30:00 AM 6
        /// 1/8/1985 10:30:00 AM 7
        /// 1/9/1985 10:30:00 AM 8
        /// 1/10/1985 10:30:00 AM 9
        /// </summary>
    [TestMethod]
    public void TimeWindowBehavior_Cover_EndShiftedLeft()
    {
      var fn = "TimeWindowBehavior_Cover_EndShiftedLeft.dss";
      File.Delete(fn);
      using (DssWriter dss = new DssWriter(fn))
      {
        var ts = CreateRegularTimeSeries();
        ts.WriteToConsole();
        dss.Write(ts);

        DateTime t1 = ts.Times[1];
        DateTime t2 = ts.Times[8].AddHours(6);

        var ts2 = dss.GetTimeSeries(ts.Path, t1, t2, TimeWindow.TimeWindowBehavior.Cover);
        ts2.WriteToConsole();
        Assert.AreEqual(ts.Times[1], ts2.Times[0]);
        Assert.AreEqual(ts.Times[9], ts2.Times[ts2.Times.Length - 1]);

        Assert.AreEqual(1.0, ts2.Values[0]);
        Assert.AreEqual(9.0, ts2.Values[ts2.Times.Length - 1]);

      }

    }

    [TestMethod]
    public void TimeWindowBehavior_Cover_StartIsBeforeTimeSeries_EndIsAfterTimeSeries()
    {
      var fn = "TimeWindowBehavior_Cover_StartIsBeforeTimeSeries_EndIsAfterTimeSeries.dss";
      File.Delete(fn);
      using (DssWriter dss = new DssWriter(fn))
      {
        var ts = CreateRegularTimeSeries();
        dss.Write(ts);

        DateTime t1 = ts.Times[1].AddHours(-1000);
        DateTime t2 = ts.Times[8].AddHours(1000);

        var ts2 = dss.GetTimeSeries(ts.Path, t1, t2, TimeWindow.TimeWindowBehavior.Cover);

        Assert.AreEqual(ts.Times[0], ts2.Times[0]);
        Assert.AreEqual(ts.Times[9], ts2.Times[ts2.Times.Length - 1]);

        Assert.AreEqual(0.0, ts2.Values[0]);
        Assert.AreEqual(9.0, ts2.Values[ts2.Times.Length - 1]);

      }

    }

       private static int SecondsInInterval(TimeSeries ts)
    {// ztsGetStandardInterval.c  -- later... TO DO.
      if (String.Compare(ts.Path.Epart, "1day", true) == 0)
        return 86400;

      return 0;
    }

    private static TimeSeries CreateRegularTimeSeries()
    {
      TimeSeries ts = new TimeSeries();
      ts.Path = new DssPath("/dotnet/csharp/flow//1Day/v1/");
      var timestamps = new List<DateTime>();
      var data = new List<double>();
      DateTime t = new DateTime(1985, 1, 1).AddHours(10).AddMinutes(30);
      for (int i = 0; i < 10; i++)
      {
        timestamps.Add(t);
        t = t.AddDays(1);
        data.Add(i);
      }
      ts.Values = data.ToArray();
      ts.Times = timestamps.ToArray();
      return ts;
    }


    [TestMethod]
    [Ignore]
    public void TestCoverIrregular()
    {
      TimeSeries ts = new TimeSeries();
      ts.Path = new DssPath("/TestCoverIrregular/csharp/flow//IR-Day/v1/");
      var timestamps = new List<DateTime>();
      timestamps.Add(new DateTime(2000, 1, 21));
      timestamps.Add(new DateTime(2002, 1, 1));
      timestamps.Add(new DateTime(2002, 1, 1));
      timestamps.Add(new DateTime(2002, 1, 2));
      timestamps.Add(new DateTime(2002, 1, 3));
      timestamps.Add(new DateTime(2003, 2, 1));
      ts.Values = new double[6] {1,2,3,4,5,6 };
      ts.Times = timestamps.ToArray();

      string fn = "TestCoverIrregular.dss";
      File.Delete(fn);
      using (DssWriter w = new DssWriter(fn))
      {
        w.Write(ts);
        DateTime t1 = new DateTime(2001, 1, 1);
        DateTime t2 = new DateTime(2002, 3, 1);
        var c = w.GetCatalog();

        var ts2 = w.GetTimeSeries(c[0], t1, t2, TimeWindow.TimeWindowBehavior.Cover);
        Assert.AreEqual(1, ts2[0].Value);
        Assert.AreEqual(6, ts2[ts2.Count - 1].Value);
      }

    }
  }
}
