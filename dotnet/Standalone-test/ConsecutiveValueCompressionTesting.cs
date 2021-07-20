using System;
using System.Text;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;
using System.Diagnostics;
using System.Linq;
using Hec.Dss;


namespace DSSUnitTests
{
  [TestClass]
  public class ConsecutiveValueCompressionTesting
  {
    [TestMethod]
    public void ZeroAndNoDataCompression()
    {
      DssPath p = new DssPath(@"//SACRAMENTO/PRECIP-INC//1Day/OBS/");
      using (DssReader r = new DssReader(TestUtility.BasePath + "sample7.dss"))
      {
        var ts = r.GetTimeSeries(p);
        var ts2 = r.GetTimeSeries(p, compression: TimeWindow.ConsecutiveValueCompression.ZeroAndNoData);
        Assert.IsTrue(ts2.Count <= ts.Count); // ts2 should have no more than 2 consecutive zero's or no data values at a time
      }
    }

    [TestMethod]
    public void NoDataCompression()
    {
      DssPath p = new DssPath(@"//SACRAMENTO/PRECIP-INC//1Day/OBS/");
      using (DssReader r = new DssReader(TestUtility.BasePath + "sample7.dss"))
      {
        var ts = r.GetTimeSeries(p);
        var ts2 = r.GetTimeSeries(p, compression: TimeWindow.ConsecutiveValueCompression.NoData);
        Assert.IsTrue(ts2.Count <= ts.Count); // ts2 should not have more than 2 no data values at a time
      }
    }

    [TestMethod]
    public void AnyValueCompression()
    {
      DssPath p = new DssPath(@"//SACRAMENTO/PRECIP-INC//1Day/OBS/");
      using (DssReader r = new DssReader(TestUtility.BasePath + "sample7.dss"))
      {
        var ts = r.GetTimeSeries(p);
        var ts2 = r.GetTimeSeries(p, compression: TimeWindow.ConsecutiveValueCompression.AnyValue);
        Assert.IsTrue(ts2.Count <= ts.Count); // ts2 should not have more than 2 of the same value at a time
      }
    }
  }
}
