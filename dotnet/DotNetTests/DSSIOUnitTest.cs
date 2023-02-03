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
  /// <summary>
  /// Summary description for UnitTest1
  /// </summary>
  [TestClass]
  public class DSSIOUnitTest
  {

    public DSSIOUnitTest()
    {
    }

    private TestContext testContextInstance;

    /// <summary>
    ///Gets or sets the test context which provides
    ///information about and functionality for the current test run.
    ///</summary>
    public TestContext TestContext
    {
      get
      {
        return testContextInstance;
      }
      set
      {
        testContextInstance = value;
      }
    }


    [TestMethod]
    public void GenerateRandomRecordsAndRead()
    {
      var dssFile = TestUtility.GetCopyForTesting("version7AlbersGridsTimeSeries.dss");
      GenerateRecords(dssFile);
      RunReadTest(dssFile);
    }

    [TestMethod]
    [Ignore]
    public void Try1SecondEPart()
    {
      string dssFile = TestUtility.GetCopyForTesting("version7AlbersGrid.dss");
      string path = @"/SHG/TRUCKEE RIVER/TEMP-AIR/31JAN2016:2400//INTERPOLATED-ROUNDED/";
      using (DssWriter dss = new DssWriter(dssFile))
      {
        double[] values = new double[121];
        for (int i = 0; i < values.Length; i++)
        {
          values[i] = 10 + i;
        }
        var dssPath = new DssPath(path);
        dssPath.Epart = "1Second";
        TimeSeries ts = new TimeSeries(dssPath.FullPath, values, dssPath.GetDPartAsDateTime(), "lol", "INST-VAL");
        dss.Write(ts, true);
      }
    }


    [TestMethod]
    public void TestGetRecordType()
    {
      string dssFileName = TestUtility.GetCopyForTesting("sample7.dss");

      using (DssReader r = new DssReader(dssFileName))
      {
        var catalog = r.GetCatalog(true);
        var path = catalog.GetCondensedPath(new DssPath("//SACRAMENTO/PRECIP-INC/11Jul1877 - 30Jun2009/1Day/OBS/"));
        var recordType = r.GetRecordType(path);
        Assert.AreEqual(RecordType.RegularTimeSeries,recordType);
      }
    }

    [TestMethod]
    public void TestGetRecordTypeNoDPart()
    {
      var dssFile = TestUtility.GetCopyForTesting("sample7.dss");
      using (DssReader r = new DssReader(dssFile))
      {
        var recordType = r.GetRecordType(new DssPath("//SACRAMENTO/PRECIP-INC//1Day/OBS/"));
        Assert.IsTrue(recordType == RecordType.RegularTimeSeries);
      }
    }

    [TestMethod]
    [Ignore]
    public void WriteGridTest()
    {
      Assert.IsTrue(false, "Grid Not implemented");
      var dssFile = TestUtility.GetCopyForTesting("version7AlbersGridsTimeSeries.dss");
      RunWriteTest(dssFile);
    }

    [TestMethod]
    public void ReadGrid()
    {
      Assert.IsTrue(false, "Grid Not implemented");
      var dssFile = TestUtility.GetCopyForTesting("version7AlbersGridsTimeSeries.dss");
      string path = @"/SHG/TRUCKEE RIVER/TEMP-AIR/31JAN2016:2400//INTERPOLATED-ROUNDED/";
      using (DssWriter dss = new DssWriter(dssFile))
      {
        Grid grid = dss.GetGrid(path, true);
        Assert.IsNotNull(grid);

        string s = grid.SRSDefinition;
        Console.WriteLine(s);
      }
    }
    
    [TestMethod]
    public void GridTest7PlayWithCase()
    {
      var fn = Path.Combine(TestUtility.BasePath, "ras", "forecast7.dss");

      var path = "/Shg/MARFC/precip/20JAN2023:0800/20JAN2023:0900/Q0/";
      using (DssReader reader = new DssReader(fn))
      {
        var dssPath = new DssPath(path);
        var catalog = reader.GetCatalog().Select(xp =>xp.FullPath).ToList();
        var list = PathAssist.FilterByPart(catalog, dssPath);
          Assert.AreEqual(1, list.Count); 
        DssPath p = new DssPath(path);
        var grid = reader.GetGrid(p, true);

        Assert.IsNotNull(grid);
        Assert.IsNotNull(grid.Data);
      }
    }

    [TestMethod]
    public void WriteGrid()
    {
      Assert.IsTrue(false, "Grid Not implemented");
      var dssFile = TestUtility.GetCopyForTesting("version7AlbersGrid.dss");
      string newPath = @"/LOL/MYOWN/TEMP-AIR/31JAN2016:2400//GRID/";
      using (DssWriter dss = new DssWriter(dssFile))
      {
        float[] data = new float[100];
        Random rng = new Random();
        for (int i = 0; i < 100; i++)
        {
          data[i] = (float)rng.NextDouble();
        }
        Assert.Fail();
        Grid grid = null;// = dss.CreateNewSpecifiedGrid(newPath, data, 0, 0, 10, 10, sizeof(float), Grid.EDataType.INST_CUM, "mm", DateTime.Now, DateTime.Now, Grid.CompressionMethod.ZLIB_DEFLATE, "PROJCS[\"UTM_ZONE_16N_WGS84\",GEOGCS[\"WGS_84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS84\", 6378137.0, 298.257223563]],PRIMEM[\"Greenwich\", 0],UNIT[\"degree\", 0.01745329251994328]],UNIT[\"Meter\", 1.0],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"latitude_of_origin\", 0],PARAMETER[\"central_meridian\", -87],PARAMETER[\"scale_factor\", 0.9996],PARAMETER[\"false_easting\", 500000],PARAMETER[\"false_northing\", 0],AXIS[\"Easting\", EAST],AXIS[\"Northing\", NORTH]]");
        dss.StoreGrid(newPath, grid);
      }
    }

    [TestMethod]
    public void GetHeaderInformationTimeSeries()
    {
      string dssFile = TestUtility.GetCopyForTesting(@"benchmarks6\BaldEDmbrk7.dss");
      string path = "/BALD EAGLE LOC HAV/105178.6/FLOW-CUM/17Feb1999-23Feb1999/1Minute/DAMBRKSIMBRCH/";
      using (DssReader dss = new DssReader(dssFile))
      {
        DssPathCollection paths = dss.GetCatalog(true);
        var dsspath = paths.FindExactPath(path);
        var ts = dss.GetEmptyTimeSeries(dsspath);
        Assert.IsTrue(ts.Units == "ACRE-FT");
        Assert.IsTrue(ts.DataType == "INST-CUM");
      }
    }

    [TestMethod]
    public void GetHeaderInformationPairedData()
    {
      string dssFile = TestUtility.GetCopyForTesting(@"benchmarks6\BaldEDmbrk7.dss");
      string path = @"/BALD EAGLE LOC HAV//LOCATION-ELEV//19FEB1999 2055/DAMBRKSIMBRCH/";
      using (DssReader dss = new DssReader(dssFile))
      {
        DssPathCollection paths = dss.GetCatalog();
        var dsspath = paths.FindExactPath(path);
        // read meta-data only.
        var pd = dss.GetPairedData(dsspath.FullPath,true);
        Assert.AreEqual("FEET", pd.UnitsDependent);
        Assert.AreEqual("UNT", pd.TypeDependent);
        Assert.AreEqual("Riv Sta",pd.UnitsIndependent);

        // read full record
        pd = dss.GetPairedData(dsspath.FullPath);
        Assert.AreEqual("FEET",pd.UnitsDependent );
        Assert.AreEqual("UNT",pd.TypeDependent);
        Assert.AreEqual("Riv Sta", pd.UnitsIndependent);

        Assert.AreEqual(178, pd.Ordinates.Length);
        Assert.AreEqual(1, pd.CurveCount);
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

        var col1 = new double[] {20912.98,18679.56,15956.85,14054.34,12272.15,10074.43,8505.39,6981.07,4893.24,3529.99,3009.38,2652.78};
        var col2 = new double[] { 24993.01, 21581.48, 17745.02, 15239.98, 13022.79, 10438.13, 8692.51, 7057.34, 4893.24, 3499.13, 2960.99, 2586.76 };
        var col3 = new double[] { 30978.08, 26919.98, 22142.58, 18927.51, 16019.20, 12590.91, 10270.00, 8142.02, 5507.09, 4000.26, 3457.57, 3090.23 };
        var col4 = new double[] { 16020.89, 14562.59, 12741.07, 11434.04, 10178.26, 8576.13, 7383.06, 6165.35, 4340.02, 3021.05, 2504.77, 2153.57 };

        var c = Comparer<double>.Create((x, y) => Math.Abs(x - y)<0.01 ? 0:-1) ;
        CollectionAssert.AreEqual(expectedX, pd.Ordinates,c);
        CollectionAssert.AreEqual(col1, pd.Values[0],c);
        CollectionAssert.AreEqual(col2, pd.Values[1],c);
        CollectionAssert.AreEqual(col3, pd.Values[2],c);
        CollectionAssert.AreEqual(col4, pd.Values[3],c);

      }
    }

      [TestMethod]
    public void GetHeaderInformationGrid()
    {
      Assert.IsTrue(false, "Grid Not implemented");
      string dssFile = TestUtility.GetCopyForTesting("containsGrids7.dss");
      string path = @"/SHG/LAKE WINNEBAGO/PRECIP/01JUN2016:0600/01JUN2016:1200/WPC-QPF/";
      using (DssReader dss = new DssReader(dssFile))
      {
        DssPathCollection paths = dss.GetCatalog();
        var dsspath = paths.FindExactPath(path);
        var grid = dss.GetGrid(dsspath, false);
        Console.WriteLine("");
        Assert.IsTrue(grid.DataUnits == "mm");
        Assert.IsTrue(grid.DataType.ToString() == "PER_CUM");
      }
    }

    [TestMethod]
    public void CheckIfComparisonWorks()
    {
      var path1 = new DssPath("A", "", "", "", "", "");
      var path2 = new DssPath("B", "", "", "", "", "");
      Assert.IsTrue(path1.CompareTo(path2) < 0);

      var path3 = new DssPath("A", "A", "", "", "", "");
      var path4 = new DssPath("A", "B", "", "", "", "");
      Assert.IsTrue(path3.CompareTo(path4) < 0);

      var paths = new List<DssPath>();
      paths.Add(path4);
      paths.Add(path1);
      paths.Add(path2);
      paths.Add(path3);

      paths = paths.OrderBy(q => q).ToList();
      Assert.IsTrue(paths[0] == path1);
      Assert.IsTrue(paths[1] == path3);
      Assert.IsTrue(paths[2] == path4);
      Assert.IsTrue(paths[3] == path2);
    }

    [TestMethod]
    public void CheckGetCorrespondingCondensedPath()
    {
      string dssFile = TestUtility.GetCopyForTesting(@"benchmarks6\Ark7.dss");
      DssPath path = new DssPath(@"/A34-01 A34-01/-2600.0/FLOW/01JAN2003/1DAY/VER 6.1.3 03-05/");
      using (DssReader reader = new DssReader(dssFile))
      {
        DssPathCollection paths = reader.GetCatalog();
        DssPathCondensed conPath = paths.GetCondensedPath(path);
        Assert.IsTrue(conPath != null);
      }
    }

    [TestMethod]
    public void CheckIfDPartComingOutAsDateTimeNoRange()
    {
      var dssFile = TestUtility.GetCopyForTesting("version7AlbersGrid.dss");
      string path = @"/SHG/TRUCKEE RIVER/TEMP-AIR/31JAN2016:2400//INTERPOLATED-ROUNDED/";
      using (DssReader dss = new DssReader(dssFile))
      {
        DssPathCollection paths = dss.GetCatalog();
        var dsspath = paths.FindExactPath(path);
        if (dsspath != DssPath.NotFound)
        {
          var dpartdt = dsspath.GetDPartAsDateTime();
          Assert.IsTrue(dpartdt.Month == 2);
          Assert.IsTrue(dpartdt.Day == 1);
          Assert.IsTrue(dpartdt.Year == 2016);
          Assert.IsTrue(dpartdt.Hour == 0);
          Assert.IsTrue(dpartdt.Minute == 0);
        }
        else
        {
          Assert.Fail();
        }
      }
    }

    [TestMethod]
    public void CheckIfDPartComingOutAsDateTimeWithRange()
    {
      string dssFile = TestUtility.GetCopyForTesting("version7AlbersGridsTimeSeries.dss");
      string path = @"//BERLIN/FLOW/01May2016-01Jun2016/1Hour/Q0W0/";
      using (DssReader dss = new DssReader(dssFile))
      {
        DssPathCollection paths = dss.GetCatalog();
        var dsspath = paths.FindExactPath(path);
        if (dsspath != DssPath.NotFound)
        {
          var dpartdt = (dsspath as DssPathCondensed).GetDPartAsDateTimeRange();
          Assert.IsTrue(dpartdt.Item1.Month == 5);
          Assert.IsTrue(dpartdt.Item1.Day == 1);
          Assert.IsTrue(dpartdt.Item1.Year == 2016);
          Assert.IsTrue(dpartdt.Item2.Month == 6);
          Assert.IsTrue(dpartdt.Item2.Day == 1);
          Assert.IsTrue(dpartdt.Item2.Year == 2016);
        }
        else
        {
          Assert.Fail();
        }
      }
    }


    private static void GenerateRecords(string dssFile)
    {
      string[] possibleCParts = { "%", "Area", "Code", "Coeff", "Conc", "Cond", "Count", "Currency", "Current", "Depth", "Dir", "Dist", "Elev", "Energy", "Evap", "EvapRate", "Fish", "Flow", "Frost", "Head", "Height", "Irrad", "Length", "Opening", "Power", "Precip", "Pres", "Rad", "Ratio", "Rotation", "Speed", "SpinRate", "Stage", "Stor", "Temp", "Thick", "Timig", "Travel", "Turb", "TurbF", "TurbJ", "TurbN", "Volt", "Volume", "Width", "pH" };
      string[] possibleUnits = { "%", "ft2", "n/a", "n/a", "ppm", "umho/cm", "unit", "$", "ampere", "in", "deg", "mi", "ft", "MWH", "in", "in/day", "unit", "cfs", "in", "ft", "ft", "langley/min", "ft", "ft", "MW", "in", "in-hg", "langley", "n/a", "deg", "mph", "rpm", "ft", "ac-ft", "F", "in", "sec", "mi", "JTU", "FNU", "JTU", "NTU", "volt", "ft3", "ft", "su" };
      using (DssWriter dss = new DssWriter(dssFile))
      {
        Random rng = new Random();
        DateTime start = new DateTime(1969, 1, 1);
        DateTime randomDate = generateRandomDate(rng, start);
        for (int i = 0; i < 100; i++)
        {
          int cPartchosen = rng.Next(0, possibleCParts.Length);
          string CPart = possibleCParts[cPartchosen];
          string pathname = GenerateRandomPathName(CPart, turnIntoAcceptableDate(randomDate.ToString("MM-dd-yyyy")), rng);
          dss.Write(new TimeSeries(pathname, new double[300], randomDate, possibleUnits[cPartchosen], "INST-VAL"));
        }
      }
    }

    private static string turnIntoAcceptableDate(string v)
    {
      string[] split = v.Split('-');
      string month = "";
      if (split[0] == "01")
        month = "Jan";
      else if (split[0] == "02")
        month = "Feb";
      else if (split[0] == "03")
        month = "Mar";
      else if (split[0] == "04")
        month = "Apr";
      else if (split[0] == "05")
        month = "May";
      else if (split[0] == "06")
        month = "Jun";
      else if (split[0] == "07")
        month = "Jul";
      else if (split[0] == "08")
        month = "Aug";
      else if (split[0] == "09")
        month = "Sep";
      else if (split[0] == "10")
        month = "Oct";
      else if (split[0] == "11")
        month = "Nov";
      else
        month = "Dec";
      return split[1] + month + split[2];
    }

    private static string GenerateRandomPathName(string cpart, string dateTime, Random rng)
    {
      string[] possibleAParts = { "Cathedral", "Marsh", "Establishment", "Alley", "Couch", "Comet", "Playground", "Coast" };
      string possibleBPart = "Location";

      string[] possibleEParts = { "1Second", "1Minute", "1Hour", "1Day", "1Week", "1Month", "1Year" };
      string F = ((char)rng.Next(0, 128)).ToString();
      while (F == "/")
        F = ((char)rng.Next(0, 128)).ToString();

      return "/" + possibleAParts[rng.Next(0, possibleAParts.Length)] + "/" + possibleBPart + "/" + cpart + "/" + dateTime + "/" + possibleEParts[rng.Next(0, possibleEParts.Length)] + "/" + F + "/";
    }

    private static DateTime generateRandomDate(Random gen, DateTime start)
    {

      int range = (DateTime.Today - start).Days;
      return start.AddDays(gen.Next(range));
    }
    private static void RunReadTest(string dssFile)
    {
      using (DssReader dss = new DssReader(dssFile))
      {
        DssPathCollection paths = dss.GetCatalog();
        var pathsFound = paths.FilterByPart();
        string path = pathsFound[0].FullPath;
      }
    }
    private static void RunWriteTest(string dssFile)
    {
      using (DssWriter dss = new DssWriter(dssFile))
      {
        DssPathCollection paths = dss.GetCatalog();
        var pathsFound = paths.FilterByPart("Basin", "Location", "Flow", "", "1Hour", "C Test");
        DssPath path = pathsFound[0];
        //string path = PathBuilder.FormPathName("Basin", "Location", "Flow", "", "1Hour", "C Test");
        double[] values = new double[300];

        for (int i = 0; i < 300; i++)
          values[i] = (double)i;
        DateTime dateTime = new DateTime(2001, 1, 21, 12, 0, 0, 0);
        TimeSeries ts = new TimeSeries();
        ts.Path = path;
        ts.Values = values;
        ts.StartDateTime = dateTime;
        ts.Units = "cfs";
        ts.DataType = "Inst-Val";
        //dss.StoreTimeSeriesRegular(path, values, 0, dateTime, "cfs", "Inst-Val");
        dss.Write(ts, true);
        //dss.StoreTimeSeriesRegular("/Basin/Location/Flow//1Hour/C Test/", values, 0, "21Jan2001", "1200", "cfs", "Inst-Val");

        ts = dss.GetTimeSeries(path);
        LocationInformation data = dss.GetLocationInfo(path.FullPath);
        Console.ReadLine();
      }
    }
  }
}
