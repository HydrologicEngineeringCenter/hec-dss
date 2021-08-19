using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Hec.Dss;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using static Hec.Dss.Quality;

namespace DSSUnitTests
{
  [TestClass]
  public class QualityTests
  {
    [TestMethod]
    public void QualityUsageWithTimeSeriesPoint()
    {
      string fn = TestUtility.BasePath + "sample7.dss";
      TimeSeriesPoint pt;
      using (DssWriter w = new DssWriter(fn))
      {
        TimeSeries ts = w.GetTimeSeries(new DssPath("/EF RUSSIAN/COYOTE/FLOW-RES IN/01Mar2006/1Hour/SMOOTH/"));
        pt = ts[0];
        if( !IsQualitySet(pt, BaseQualityFlags.Okay) || IsQualitySet(pt, BaseQualityFlags.Protected | BaseQualityFlags.Okay))
          pt.SetQuality(BaseQualityFlags.Protected | BaseQualityFlags.Okay);
      }
      Assert.AreEqual(pt.Quality, "Okay, Protected, OriginalValue, NoRevision");

       Assert.IsTrue(IsQualitySet(pt, BaseQualityFlags.Okay));
       Assert.IsTrue(IsQualitySet(pt, BaseQualityFlags.Protected));

/* 
      if( pt.ModifiedBy == QualityModified.OriginalValue )
      {

      }
      pt.ModifiedBy = Modified.DATVUE;
    pt.ReplacementMethod = ReplacementMethods.NoRevision;
*/

    }

    [TestMethod]
    public void QualitySizeMatchesValueSize()
    {
      string fn = TestUtility.BasePath + "Russian_TimeSeries.dss";
      using (DssWriter w = new DssWriter(fn))
      {
        TimeSeries ts = w.GetTimeSeries(new DssPath("/AUSTIN C/CAZADERO CA/FLOW//15MIN/USGS/"));
        if (ts.Qualities == null)
          Assert.AreEqual(null, ts.Qualities);
        else
          Assert.AreEqual(ts.Qualities.Length, ts.Values.Length);
      }
    }

    [TestMethod]
    public void CheckWhichQualitiesAreSet()
    {
      string fn = TestUtility.BasePath + "Russian_TimeSeries.dss";
      using (DssWriter w = new DssWriter(fn))
      {
        TimeSeries ts = w.GetTimeSeries(new DssPath("/AUSTIN C/CAZADERO CA/FLOW//15MIN/USGS/"));
        List<string> qualities = new List<string>();
        for (int i = 0; i < 20; i++)
        {
          qualities.Add(Quality.ToString(ts.Qualities[i]));
        }
      }

    }
    
    [TestMethod]
    public void CheckWhichQualitiesAreSet2()
    {
      string fn = TestUtility.BasePath + "MathFunctions_1-2.dss";
      using (DssWriter w = new DssWriter(fn))
      {
        TimeSeries ts = w.GetTimeSeries(new DssPath("/FOX RIVER/LUTZ PARK/FLOW-RES OUT//15Minute/USGS-CST-FIXED/"));
        List<string> qualities = new List<string>();
        for (int i = 0; i < 20; i++)
        {
          qualities.Add(Quality.ToString(ts.Qualities[i]));
        }
      }
    }

    [TestMethod]
    public void CheckIfAllFlagsPropertyIsFunctional()
    {
      var l = Quality.AllFlags;

      var f = Quality.TestFailedFlags.DistributionTest;
      var f_l = l[25];

      Assert.AreEqual(f, f_l.Flag);
    }

    [TestMethod]
    public void GetFlagFromAllFlagsAndSetThatFlagInTimeSeriesPoint()
    {
      var l = Quality.AllFlags;
      TimeSeriesPoint tsp = new TimeSeriesPoint(default(DateTime), 1.0);
      tsp.SetQuality(l[0].Value);

    }

    [TestMethod]
    public void CreateFlagObject()
    {
      var f = new Quality(BaseQualityFlags.Screened);
      var f1 = new Quality(PreviousModifierFlags.DATCHK);
      var f2 = new Quality(TestFailedFlags.AbsoluteMagnitude);
      var f3 = new Quality(ReplacementMethodFlags.NoRevision);

      Assert.AreEqual(f.Value, (int)BaseQualityFlags.Screened);
      Assert.AreEqual(f1.Value, (int)PreviousModifierFlags.DATCHK);
      Assert.AreEqual(f2.Value, (int)TestFailedFlags.AbsoluteMagnitude);
      Assert.AreEqual(f3.Value, (int)ReplacementMethodFlags.NoRevision);
      
      Assert.AreEqual(f.Name, BaseQualityFlags.Screened.ToString());
      Assert.AreEqual(f1.Name, PreviousModifierFlags.DATCHK.ToString());
      Assert.AreEqual(f2.Name, TestFailedFlags.AbsoluteMagnitude.ToString());
      Assert.AreEqual(f3.Name, ReplacementMethodFlags.NoRevision.ToString());
      
      Assert.AreEqual((BaseQualityFlags)(f.Flag), BaseQualityFlags.Screened);
      Assert.AreEqual((PreviousModifierFlags)(f1.Flag), PreviousModifierFlags.DATCHK);
      Assert.AreEqual((TestFailedFlags)(f2.Flag), TestFailedFlags.AbsoluteMagnitude);
      Assert.AreEqual((ReplacementMethodFlags)(f3.Flag), ReplacementMethodFlags.NoRevision);


    }

    [TestMethod]
    public void GetQualityStats()
    {
      string fn = TestUtility.BasePath + "Russian_TimeSeries.dss";
      using (DssWriter w = new DssWriter(fn))
      {
        TimeSeries ts = w.GetTimeSeries(new DssPath("/AUSTIN C/CAZADERO CA/FLOW//15MIN/USGS/"));
        var l = ts.ToTimeSeriesPoints();
        var a = l[0];
        var q = a.QualityStats;
        var b = QualityStats(a, false, true);
        var c = QualityStats(a, true, false);

      }
    }

        [TestMethod]
        public void SimpleQualityConsistencyTestDss7()
        {
            string fn = TestUtility.BasePath + "simpleQCT7.dss";
            File.Delete(fn);
            Array q = Enum.GetValues(typeof(BaseQualityFlags));
            Random r = new Random();
            var qList = new List<int>();
            using (DssWriter w = new DssWriter(fn))
            {
                var ts = TimeSeriesTest.CreateSampleTimeSeries(new DateTime(2020, 1, 1), "cfs", "INST", size:10);
                ts.Path = new DssPath("a", "b", "c", "", E: "1Day", F:"f");
                for (int i = 0; i < ts.Values.Length; i++)
                {
                    BaseQualityFlags flag = (BaseQualityFlags)q.GetValue(r.Next(q.Length));
                    Quality newQuality = new Quality(flag);
                    qList.Add(newQuality.Value);
                }
                ts.Qualities = qList.ToArray();
                w.Write(ts);
            }

            using (DssReader reader = new DssReader(fn))
            {
                TimeSeries ts = reader.GetTimeSeries(new DssPath("/a/b/c//1Day/f/"));
                for (int i = 0; i < ts.Values.Length; i++)
                    Assert.AreEqual(ts.Qualities[i], qList[i]);
            }

            using (DssWriter w = new DssWriter(fn))
            {
                var ts = w.GetTimeSeries(new DssPath("/a/b/c//1Day/f/"));
                qList.Clear();
                for (int i = 0; i < ts.Values.Length; i++)
                {
                    BaseQualityFlags flag = (BaseQualityFlags)q.GetValue(r.Next(q.Length));
                    Quality newQuality = new Quality(flag);
                    qList.Add(newQuality.Value);
                }
                ts.Qualities = qList.ToArray();
                w.Write(ts);
            }

            using (DssReader reader = new DssReader(fn))
            {
                TimeSeries ts = reader.GetTimeSeries(new DssPath("/a/b/c//1Day/f/"));
                for (int i = 0; i < ts.Values.Length; i++)
                    Assert.AreEqual(ts.Qualities[i], qList[i]);
            }
        }
  }
}
