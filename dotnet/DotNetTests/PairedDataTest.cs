using System;
using System.Collections.Generic;
using System.IO;
using Hec.Dss;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace DSSUnitTests
{
  [TestClass]
  public class PairedDataTest
  {
    [TestMethod]
    public void ReadPairedData()
    {
      var path = "/MY BASIN/DEER CREEK/STAGE-FLOW///USGS/";
      using (DssReader r = new DssReader(TestUtility.BasePath + "sample7.dss"))
      {
        var pd = r.GetPairedData(path);


        //Assert.AreEqual()
        Assert.AreEqual("FEET", pd.UnitsIndependent);
        Assert.AreEqual("UNT", pd.TypeDependent);

        Assert.AreEqual("CFS", pd.UnitsDependent);
        Assert.AreEqual("UNT", pd.TypeDependent);

        Assert.AreEqual(0, pd.LocationInformation.CoordinateID);
      }

    }
    [TestMethod]
    public void ReadPairedDataWithXY()
    {
      var path = "/FISHKILL CREEK/BEACON NY/FREQ-FLOW///USGS/";
      using (DssReader r = new DssReader(TestUtility.BasePath + "sample7_units_xyz.dss"))
      {
        var pd = r.GetPairedData(path);
        Assert.AreEqual(123, pd.LocationInformation.XOrdinate);
        Assert.AreEqual(456, pd.LocationInformation.YOrdinate);

      }

    }

    [TestMethod]
    public void ReadPairedDataWithMultipleCurves()
    {
      var path = "/a/b/stage-flow//e/f/";
      using (DssReader r = new DssReader(TestUtility.BasePath + "pdTest1.dss"))
      {
        var pd = r.GetPairedData(path);

        Assert.AreEqual(2, pd.Values.Count);


        Assert.AreEqual(1, pd.Values[0][0]);
        Assert.AreEqual(11, pd.Values[0][1]);
        Assert.AreEqual(111, pd.Values[0][2]);
        Assert.AreEqual(1111, pd.Values[0][3]);
        Assert.AreEqual(11111, pd.Values[0][4]);

        Assert.AreEqual(2, pd.Values[1][0]);
        Assert.AreEqual(22, pd.Values[1][1]);
        Assert.AreEqual(222, pd.Values[1][2]);
        Assert.AreEqual(2222, pd.Values[1][3]);
        Assert.AreEqual(22222, pd.Values[1][4]);

        Assert.AreEqual("duck1", pd.Labels[0]);
        Assert.AreEqual("duck2", pd.Labels[1]);



        //Assert.AreEqual()
        Assert.AreEqual("ft", pd.UnitsIndependent);
        Assert.AreEqual("UNT", pd.TypeDependent);

        Assert.AreEqual("cfs", pd.UnitsDependent);
        Assert.AreEqual("UNT", pd.TypeDependent);

        Assert.AreEqual(0, pd.LocationInformation.CoordinateID);
      }

    }

    [TestMethod]
    public void ReadPairedDataWithMultipleCurves2()
    {
      var path = "/a/b/stage-flow//e/f/";
      using (DssReader r = new DssReader(TestUtility.BasePath + "pdTest1.dss"))
      {
        var pd = r.GetPairedData(path);

        Assert.AreEqual(2, pd.Values.Count);


        Assert.AreEqual(1, pd.Values[0][0]);
        Assert.AreEqual(11, pd.Values[0][1]);
        Assert.AreEqual(111, pd.Values[0][2]);
        Assert.AreEqual(1111, pd.Values[0][3]);
        Assert.AreEqual(11111, pd.Values[0][4]);

        Assert.AreEqual(2, pd.Values[1][0]);
        Assert.AreEqual(22, pd.Values[1][1]);
        Assert.AreEqual(222, pd.Values[1][2]);
        Assert.AreEqual(2222, pd.Values[1][3]);
        Assert.AreEqual(22222, pd.Values[1][4]);

        Assert.AreEqual("duck1", pd.Labels[0]);
        Assert.AreEqual("duck2", pd.Labels[1]);



        //Assert.AreEqual()
        Assert.AreEqual("ft", pd.UnitsIndependent);
        Assert.AreEqual("UNT", pd.TypeDependent);

        Assert.AreEqual("cfs", pd.UnitsDependent);
        Assert.AreEqual("UNT", pd.TypeDependent);

        Assert.AreEqual(0, pd.LocationInformation.CoordinateID);
      }
    }

    [TestMethod]
    public void GetPairedDataV7()
    {
      var path = "/a/b/stage-flow//e/f/";
      using (DssReader r = new DssReader(TestUtility.BasePath + "spd7.dss"))
      {
        var pd = r.GetPairedData(path);

        Assert.AreEqual("FEET", pd.UnitsIndependent);
        Assert.AreEqual("UNT", pd.TypeDependent);

        Assert.AreEqual("CFS", pd.UnitsDependent);
        Assert.AreEqual("UNT", pd.TypeDependent);

        var d = pd.Values[0][1];

        Assert.IsNotNull(pd);
      }
    }

    [TestMethod]
    public void WritePairedData()
    {
      var fn = "test_write_paired_data.dss";
      File.Delete(fn);
      using (var w = new DssWriter(fn))
      {
        var x = new double[] { 1.0, 2.0, 3.0 };
        var y = new double[] { 10.0, 20.0, 30.0 };

        PairedData pd = new PairedData();
        pd.Ordinates = x;
        pd.Values = new List<double[]>();
        pd.Values.Add(y);


        PairedData pd2 = new PairedData(
          new double[] { 1.0, 2.0, 3.0 }, new double[] { 10.0, 20.0, 30.0 });

        var myList = new List<double[]>(3);
        myList.Add(new double[] { 10.0, 20.0, 30.0 });
        myList.Add(new double[] { 100.0, 200.0, 300.0 });
        myList.Add(new double[] { 1000.0, 2000.0, 3000.0 });

        PairedData pd3 = new PairedData(new double[] { 1.0, 2.0, 3.0 }, myList, new List<string>(),"", "", "", "", "/a/b/stage-flow//e/f/");

        w.Write(pd3);


      }
    }

    [TestMethod]
    public void ReadPairedData2()
    {
      using (DssReader r = new DssReader(TestUtility.BasePath + "pairedData1.dss"))
      {
        var p = r.GetPairedData("/excel/import/plugin//e/pairedDataAXD/");
      }
    }

    [TestMethod]
    public void ReadPairedData3()
    {
      using (DssReader r = new DssReader(TestUtility.BasePath + "indexedPairedData1.dss"))
      {
        var p = r.GetPairedData("/excel/import/plugin//e/pairedData3CR/");
      }
    }
  }
}
