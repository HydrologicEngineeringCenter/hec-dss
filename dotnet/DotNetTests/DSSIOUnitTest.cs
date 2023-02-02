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
      //
      // TODO: Add constructor logic here
      //
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

    #region Additional test attributes
    //
    // You can use the following additional attributes as you write your tests:
    //
    // Use ClassInitialize to run code before running the first test in the class
    // [ClassInitialize()]
    // public static void MyClassInitialize(TestContext testContext) { }
    //
    // Use ClassCleanup to run code after all tests in a class have run
    // [ClassCleanup()]
    // public static void MyClassCleanup() { }
    //
    // Use TestInitialize to run code before running each test 
    // [TestInitialize()]
    // public void MyTestInitialize() { }
    //
    // Use TestCleanup to run code after each test has run
    // [TestCleanup()]
    // public void MyTestCleanup() { }
    //
    #endregion

    public static void CopyAsReadWrite(string fromFN, string toFN)
    {
      // Set permissions to allow us to delete it
      if (File.Exists(toFN))
      {
        System.IO.FileInfo fi = new System.IO.FileInfo(toFN);
        fi.IsReadOnly = false;
        File.Delete(toFN);
      }

      File.Copy(fromFN, toFN);

      // Reset the read/write permissions
      System.IO.FileInfo fileInfo = new System.IO.FileInfo(toFN);
      fileInfo.IsReadOnly = false;
    }

    [TestMethod]
    public void GenerateRandomRecordsAndRead()
    {
      const string srcfile = TestUtility.BasePath + "version7AlbersGridsTimeSeries.dss";
      const string dssFile = @".\tempdssfile.dss";
      CopyAsReadWrite(srcfile, dssFile);
      GenerateRecords(dssFile);
      RunReadTest(dssFile);
    }

    [TestMethod]
    [Ignore]
    public void Try1SecondEPart()
    {
      string dssFile = @".\tempdssfile.dss";
      string srcFile = TestUtility.BasePath + "version7AlbersGrid.dss";
      File.Delete(dssFile);
      CopyAsReadWrite(srcFile, dssFile);
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
      //File.Delete(dssFile);
    }

    //[TestMethod]
    //public void TryReadBEDSS()
    //{
    //  GDAL.ExecutableTests.InitializeGDAL();

    //  const string srcfile = TestUtility.BasePath +  @"C:\Users\q0hecajk\Documents\Testing\_Rainfall\2017.09_RAS_Coordination\DSS\precip.2017.07.dss";
    //  const string dssFile = @".\tempdssfile.dss";
    //  const string prjFile = @"C:\Users\q0hecajk\Documents\Testing\_Rainfall\2017.09_RAS_Coordination\RAS\BaldEagleCrkMulti2D\Albers_Custom.prj";
    //  const string tiff_Chemung = @"C:\Users\q0hecajk\Documents\Testing\_Rainfall\2017.09_RAS_Coordination\RAS\BaldEagleCrkMulti2D\Sample Rainfall\Chemung.tif";
    //  const string tiff_Juanita = @"C:\Users\q0hecajk\Documents\Testing\_Rainfall\2017.09_RAS_Coordination\RAS\BaldEagleCrkMulti2D\Sample Rainfall\Juanita.tif";
    //  const string tiff_mainstem = @"C:\Users\q0hecajk\Documents\Testing\_Rainfall\2017.09_RAS_Coordination\RAS\BaldEagleCrkMulti2D\Sample Rainfall\MainStemSusq.tif";
    //  const string tiff_upper = @"C:\Users\q0hecajk\Documents\Testing\_Rainfall\2017.09_RAS_Coordination\RAS\BaldEagleCrkMulti2D\Sample Rainfall\UpperSusq.tif";
    //  const string tiff_west = @"C:\Users\q0hecajk\Documents\Testing\_Rainfall\2017.09_RAS_Coordination\RAS\BaldEagleCrkMulti2D\Sample Rainfall\WestSusq.tif";

    //  CopyAsReadWrite(srcfile, dssFile);
    //  string path_chemung = @"/SHG/CHEMUNG RIVER/PRECIP/24JUL2017:0600/24JUL2017:0700/NDGD-RTMA/"; // chemung
    //  string path_juanita = @"/SHG/JUNIATA RIVER/PRECIP/24JUL2017:0600/24JUL2017:0700/NDGD-RTMA/"; // juanita
    //  string path_mainstem = @"/SHG/MAIN STEM SUSQUEHANNA RIVER/PRECIP/24JUL2017:0600/24JUL2017:0700/NDGD-RTMA/";
    //  string path_upper = @"/SHG/UPPER SUSQUEHANNA RIVER/PRECIP/24JUL2017:0600/24JUL2017:0700/NDGD-RTMA/";
    //  string path_west = @"/SHG/WEST BRANCH SUSQUEHANNA RIVER/PRECIP/24JUL2017:0600/24JUL2017:0700/NDGD-RTMA/";

    //  using (DSSWriter dss = new DSSWriter(dssFile))
    //  {
    //    Action<string, string> ProcessGridWriteTiff = (dssPath, tiffFile) =>
    //    {
    //      Grid grid = dss.GetGrid(dssPath, true);

    //      var values = grid.GriddedValues;
    //      int width = grid.NumberOfCellsX;
    //      int height = grid.NumberOfCellsY;

    //      // ALBERS grids are in 2KM units
    //      File.Delete(tiffFile);
    //      TiffAssist.FloatTiffWriter.Write(tiffFile, width, height, (r, c) => values[(height - r - 1) * width + c]);

    //      if(false)
    //      {
    //        var proj = new GDALAssist.ESRIProjection(prjFile);
    //        GDALAssist.Metadata.SetProjectionInfo(tiffFile, proj);
    //        RasMapperLib.InterpolatedLayer.AddGeoreferencing(tiffFile, grid.LowerLeftCellX * 2000, (grid.LowerLeftCellY + height) * 2000, grid.CellSize);
    //      }
    //      else
    //      {
    //        RasMapperLib.InterpolatedLayer.AddGeoreferencing(tiffFile, grid.LowerLeftCellX, (grid.LowerLeftCellY + height), 1);
    //      }
    //    };

    //    ProcessGridWriteTiff(path_chemung, tiff_Chemung);
    //    ProcessGridWriteTiff(path_juanita, tiff_Juanita);
    //    ProcessGridWriteTiff(path_mainstem, tiff_mainstem);
    //    ProcessGridWriteTiff(path_upper, tiff_upper);
    //    ProcessGridWriteTiff(path_west, tiff_west);
    //  }
    //  File.Delete(dssFile);


    //}

    [TestMethod]
    public void TestGetRecordType
      
      ()
    {
      using (DssReader r = new DssReader(TestUtility.BasePath + "sample7.dss"))
      {
        var catalog = r.GetCatalog(true);
        var path = catalog.GetCondensedPath(new DssPath("//SACRAMENTO/PRECIP-INC/11Jul1877 - 30Jun2009/1Day/OBS/"));
        var recordType = r.GetRecordType(path);
        Assert.IsTrue(recordType == RecordType.RegularTimeSeries);
      }
    }

    [TestMethod]
    public void TestGetRecordTypeNoDPart()
    {
      using (DssReader r = new DssReader(TestUtility.BasePath + "sample7.dss"))
      {
        var recordType = r.GetRecordType(new DssPath("//SACRAMENTO/PRECIP-INC//1Day/OBS/"));
        Assert.IsTrue(recordType == RecordType.RegularTimeSeries);
      }
    }

    [TestMethod]
    [Ignore]
    public void WriteGridTest()
    {
      string dssFile = @".\tempdssfile.dss";
      File.Delete(dssFile);
      const string srcfile = TestUtility.BasePath + "version7AlbersGridsTimeSeries.dss";
      CopyAsReadWrite(srcfile, dssFile);
      RunWriteTest(dssFile);
    }

    [TestMethod]
    [Ignore]
    public void ReadGrid()
    {
      string dssFile = @".\tempdssfile.dss";
      string srcFile = TestUtility.BasePath + "version7AlbersGrid.dss";
      File.Delete(dssFile);
      CopyAsReadWrite(srcFile, dssFile);
      string path = @"/SHG/TRUCKEE RIVER/TEMP-AIR/31JAN2016:2400//INTERPOLATED-ROUNDED/";
      using (DssWriter dss = new DssWriter(dssFile))
      {
        Grid grid = dss.GetGrid(path, true);
        Assert.IsNotNull(grid);

        string s = grid.SRSDefinition;
        Console.WriteLine(s);
      }
      File.Delete(dssFile);
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
    [Ignore]
    public void WriteGrid()
    {
      string dssFile = @".\tempdssfile.dss";
      string srcFile = TestUtility.BasePath + "version7AlbersGrid.dss";
      File.Delete(dssFile);
      CopyAsReadWrite(srcFile, dssFile);
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
      File.Delete(dssFile);
    }

    [TestMethod]
    public void GetHeaderInformationTimeSeries()
    {
      string dssFile = @".\tempdssfile.dss";
      string srcFile = TestUtility.BasePath + @"benchmarks6/BaldEDmbrk.dss";
      File.Delete(dssFile);
      CopyAsReadWrite(srcFile, dssFile);
      string path = @"/BALD EAGLE LOC HAV/105178.6/FLOW-CUM/17FEB1999-23FEB1999/1MIN/DAMBRKSIMBRCH/";
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
      string dssFile = @".\tempdssfile.dss";
      string srcFile = TestUtility.BasePath + @"benchmarks6\BaldEDmbrk.dss";
      File.Delete(dssFile);
      CopyAsReadWrite(srcFile, dssFile);
      string path = @"/BALD EAGLE LOC HAV//LOCATION-ELEV//19FEB1999 2055/DAMBRKSIMBRCH/";
      using (DssReader dss = new DssReader(dssFile))
      {
        DssPathCollection paths = dss.GetCatalog();
        var dsspath = paths.FindExactPath(path);
        var pd = dss.GetPairedData(dsspath.FullPath);
        Assert.IsTrue(pd.UnitsDependent == "FEET");
        Assert.IsTrue(pd.TypeDependent == "UNT");
      }
    }

    [TestMethod]
    public void GetHeaderInformationGrid()
    {
      string dssFile = @".\tempdssfile.dss";
      string srcFile = TestUtility.BasePath + @"containsGrids7.dss";
      File.Delete(dssFile);
      CopyAsReadWrite(srcFile, dssFile);
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
      string dssFile = @".\tempdssfile.dss";
      string srcFile = TestUtility.BasePath + @"benchmarks6\Ark.dss";
      File.Delete(dssFile);
      CopyAsReadWrite(srcFile, dssFile);
      DssPath path = new DssPath(@"/A34-01 A34-01/-2600.0/FLOW/01JAN2003/1DAY/VER 6.1.3 03-05/");
      using (DssReader reader = new DssReader(dssFile))
      {
        DssPathCollection paths = reader.GetCatalog();
        DssPathCondensed conPath = paths.GetCondensedPath(path);
        Assert.IsTrue(conPath != null);
      }

    }

    [TestMethod]
    [Ignore]
    public void CheckIfDPartComingOutAsDateTimeNoRange()
    {
      string dssFile = @".\tempdssfile.dss";
      string srcFile = TestUtility.BasePath + "version7AlbersGrid.dss";
      File.Delete(dssFile);
      CopyAsReadWrite(srcFile, dssFile);
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
      string dssFile = @".\tempdssfile.dss";
      string srcFile = TestUtility.BasePath + "version7AlbersGridsTimeSeries.dss";
      File.Delete(dssFile);
      CopyAsReadWrite(srcFile, dssFile);
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
