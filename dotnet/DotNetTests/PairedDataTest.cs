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
      using (DssReader r = new DssReader(TestUtility.GetCopyForTesting( "sample7.dss")))
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
      using (DssReader r = new DssReader(TestUtility.GetCopyForTesting("sample7_units_xyz.dss")))
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
      using (DssReader r = new DssReader(TestUtility.GetCopyForTesting("pdTest1.dss")))
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
      using (DssReader r = new DssReader(TestUtility.GetCopyForTesting("pdTest1.dss")))
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
      using (DssReader r = new DssReader(TestUtility.GetCopyForTesting("spd7.dss")))
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
      var fn = TestUtility.GetSimpleTempFileName(".dss");
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
      using (DssReader r = new DssReader(TestUtility.GetCopyForTesting("pairedData1.dss")))
      {
        var p = r.GetPairedData("/excel/import/plugin//e/pairedDataAXD/");
      }
    }

    [TestMethod]
    public void ReadPairedData3()
    {
      using (DssReader r = new DssReader(TestUtility.GetCopyForTesting("indexedPairedData1.dss")))
      {
        var p = r.GetPairedData("/excel/import/plugin//e/pairedData3CR/");
      }
    }
    [TestMethod]
    public void PairedDataWithLabels()
    {
      string dssFile = TestUtility.GetCopyForTesting("examples-all-data-types.dss");
      using (DssReader dss = new DssReader(dssFile))
      {
        var pd = dss.GetPairedData("/paired-data-multi-column/RIVERDALE/FREQ-FLOW/MAX ANALYTICAL//1969-01 H33(MAX)/");
        Assert.AreEqual("PERCENT", pd.UnitsIndependent);
        Assert.AreEqual("FREQ", pd.TypeIndependent);

        Assert.AreEqual("CFS", pd.UnitsDependent);
        Assert.AreEqual("FLOW", pd.TypeDependent);
        CollectionAssert.AreEqual(new String[] { "COMPUTED", "EXP PROB", "5%LIMIT", "95%LIMIT" },
                   pd.Labels.ToArray());
        Assert.AreEqual(12, pd.Ordinates.Length);
        var expectedX = new double[] { 0.100, 0.200, 0.500, 1.000, 2.000, 5.000, 10.00, 20.00, 50.00, 80.00, 90.00, 95.00 };

        var col1 = new double[] { 20912.98, 18679.56, 15956.85, 14054.34, 12272.15, 10074.43, 8505.39, 6981.07, 4893.24, 3529.99, 3009.38, 2652.78 };
        var col2 = new double[] { 24993.01, 21581.48, 17745.02, 15239.98, 13022.79, 10438.13, 8692.51, 7057.34, 4893.24, 3499.13, 2960.99, 2586.76 };
        var col3 = new double[] { 30978.08, 26919.98, 22142.58, 18927.51, 16019.20, 12590.91, 10270.00, 8142.02, 5507.09, 4000.26, 3457.57, 3090.23 };
        var col4 = new double[] { 16020.89, 14562.59, 12741.07, 11434.04, 10178.26, 8576.13, 7383.06, 6165.35, 4340.02, 3021.05, 2504.77, 2153.57 };

        var c = Comparer<double>.Create((x, y) => Math.Abs(x - y) < 0.01 ? 0 : -1);
        CollectionAssert.AreEqual(expectedX, pd.Ordinates, c);
        CollectionAssert.AreEqual(col1, pd.Values[0], c);
        CollectionAssert.AreEqual(col2, pd.Values[1], c);
        CollectionAssert.AreEqual(col3, pd.Values[2], c);
        CollectionAssert.AreEqual(col4, pd.Values[3], c);

      }
    }


  }
}
