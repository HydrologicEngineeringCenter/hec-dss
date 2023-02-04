using System;
using System.IO;
using System.Text.RegularExpressions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Hec.Dss;
using System.Linq;

namespace DSSUnitTests
{
  [Ignore]
  [TestClass]
  public class GridTesting
  {
    [TestMethod]
    public void ReadBugUTM()
    {
      string fn = TestUtility.BasePath + "BALDEAGLE.DSS";
      string path = "/UTM_18N/MARFC/PRECIP/23JUL2003:0000/23JUL2003:0100/NEXRAD (1000 M)/";
      using (DssReader r = new DssReader(fn))
      {
        Grid g1 = r.GetGrid(path, false);
        Console.WriteLine(g1.SRSDefinition);

      }
    }


    [TestMethod]
    public void ReadMetVueHRAP()
    {
      string path = "/HRAP/MARFC/PRECIP/23JUL2003:1800/23JUL2003:1900/NEXRAD/";

      string fn = TestUtility.BasePath + "BALDEAGLE.DSS";
      using (DssReader r = new DssReader(fn))
      {
        Grid g1 = r.GetGrid(path, false);
        Console.WriteLine(g1.SRSDefinition);

        string hrap_wkt = @"PROJCS[""Stereographic_CONUS_HRAP"",
GEOGCS[""GCS_Sphere_LFM"",DATUM[""D_Sphere_LFM"",
SPHEROID[""Shpere_LFM"",6371200.0,0.0]],PRIMEM[""Greenwich"",0.0],
UNIT[""Degree"",0.0174532925199433]],
PROJECTION[""Stereographic_North_Pole""],
PARAMETER[""False_Easting"",1909762.5],PARAMETER[""False_Northing"",7624762.5],
PARAMETER[""Central_Meridian"",-105.0],PARAMETER[""Standard_Parallel_1"",60.0],
UNIT[""Meter"",1.0]]";
        hrap_wkt = Regex.Replace(hrap_wkt, @"\s+", "");
        Console.WriteLine(g1.SRSDefinition);
        Assert.AreEqual(hrap_wkt, g1.SRSDefinition);
      }

    }

    [TestMethod]
    public void ReadSpecifiedGrid()
    {
      ReadSpecifiedGrid("radar_utm.dss");
      ReadSpecifiedGrid("radar_utm7.dss");

    }
    public void ReadSpecifiedGrid(string filename)
    {
      string path = "/UTM16N/PISTOL/PRECIP/05MAY2003:1100/05MAY2003:1200/RADAR/";
      string fn = TestUtility.BasePath + filename;
      using (DssReader r = new DssReader(fn))
      {

        Grid g1 = r.GetGrid(path, false);
        Grid g2 = r.GetGrid(path, true);

        string javaSRS = @"PROJCS[""UTM_ZONE_16N_WGS84"",
		GEOGCS[""WGS_84"",
			DATUM[""WGS_1984"",
				SPHEROID[""WGS84"",6378137.0,298.257223563]],
			PRIMEM[""Greenwich"",0],
			UNIT[""degree"",0.01745329251994328]],
		UNIT[""Meter"",1.0],
		PROJECTION[""Transverse_Mercator""],
		PARAMETER[""latitude_of_origin"",0],
		PARAMETER[""central_meridian"",-87],
		PARAMETER[""scale_factor"",0.9996],
		PARAMETER[""false_easting"",500000],
		PARAMETER[""false_northing"",0],
		AXIS[""Easting"",EAST],
		AXIS[""Northing"",NORTH]]";
        javaSRS = Regex.Replace(javaSRS, @"\s+", "");

        Assert.AreEqual(javaSRS, g1.SRSDefinition);

        Assert.IsNotNull(g1);
        Assert.IsNotNull(g2);
        //    /UTM16N/PISTOL/PRECIP/05MAY2003:1100/05MAY2003:1200/RADAR/

        //  Grid Type: SPECIFIED SPATIAL REFERENCE SYSTEM
        float delta = 0.000001f; // tolarance for comparisons;
        Assert.AreEqual(DssDataType.PER_CUM, g2.DataType);
        Assert.AreEqual(48, g2.Data.Length);
        Assert.AreEqual(384, g2.LowerLeftCellX);
        Assert.AreEqual(1975, g2.LowerLeftCellY);
        Assert.AreEqual("mm", g2.DataUnits.ToLower());
        Assert.AreEqual(6, g2.NumberOfCellsX);
        Assert.AreEqual(8, g2.NumberOfCellsY);
        Assert.AreEqual(2000.0f, g2.CellSize, delta);

        Assert.AreEqual(2.6322386f, g2.MaxDataValue, delta);
        Assert.AreEqual(1.3286265f, g2.MinDataValue, delta);
        Assert.AreEqual(1.6439365f, g2.MeanDataValue, delta);
        Assert.AreEqual(0.0f, g2.XCoordOfGridCellZero, delta);
        Assert.AreEqual(0.0f, g2.YCoordOfGridCellZero, delta);


      }

    }


    private static Grid SampleGrid()
    {
      var g = new Grid();
      g.GridType = GridType.ALBERS;

      /*//sb.AppendLine("Storage Data Type  :"+ _grid.StorageDataType);
      sb.AppendLine("GridStruct Version :" + StructVersion);
      sb.AppendLine("Path :" + PathName);
      sb.AppendLine("GridType :" + GridType);
      sb.AppendLine("Version : " + Version);
      sb.AppendLine("Data Units : " + DataUnits);
      sb.AppendLine("Data Type : " + DataType);
      sb.AppendLine("Data Source : " + StorageDataType);
      sb.AppendLine("LowerLeftCellX : " + LowerLeftCellX);
      sb.AppendLine("LowerLeftCellY : " + LowerLeftCellY);
      sb.AppendLine("NumberOfCellsX : " + NumberOfCellsX);
      sb.AppendLine("NumberOfCellsY : " + NumberOfCellsY);
      sb.AppendLine("CellSize : " + CellSize.ToString("0.00000"));
      sb.AppendLine("CompressionMethod : " + CompressionMethod);
      sb.AppendLine("SizeofCompressedElements : " + SizeOfCompressedElements);

      sb.AppendLine("NumberOfRanges : " + NumberOfRanges);
      sb.AppendLine("SrsName : " + SRSName);
      sb.AppendLine("SrsDefinitionType : " + SRSDefinitionType);
      sb.AppendLine("_srsDefinition : " + SRSDefinition);
      sb.AppendLine("XCoordOfGridCellZero : " + XCoordOfGridCellZero.ToString("0.00000"));
      sb.AppendLine("YCoordOfGridCellZero : " + YCoordOfGridCellZero.ToString("0.00000"));
      sb.AppendLine("NullValue : " + NullValue.ToString("0.00000"));
      sb.AppendLine("TimeZoneID : " + TimeZoneID);
      sb.AppendLine("timeZoneRawOffset : " + TimeZoneRawOffset);
      sb.AppendLine("IsInterval : " + IsInterval);
      sb.AppendLine("IsTimeStamped : " + IsTimeStamped);
      sb.AppendLine("Storage Data Type  :" + StorageDataType);

      if (StorageDataType == 0) // GRID_FLOAT
      {
        sb.AppendLine("Max Data Value : " + MaxDataValue.ToString("0.00000"));
        sb.AppendLine("Min Data Value : " + MinDataValue.ToString("0.00000"));
        sb.AppendLine("Mean Data Value : " + MeanDataValue.ToString("0.00000"));
        sb.AppendLine("======== Range Limit Table ===========");

        if (RangeLimitTable != null && RangeLimitTable.Length > 0)
        {
          sb.AppendLine("           Range        > or =    Incremental Count");
          int size = NumberOfRanges;
          for (int i = 0; i < size - 1; i++)
          {
            var v = RangeLimitTable[i];
            var vs = v.ToString("0.00000");
            if (v == UndefinedValue)
              vs = "undefined".PadRight(16);
            sb.AppendLine(vs + " "
                + NumberEqualOrExceedingRangeLimit[i].ToString("0.00000"));
          }

      */
          return g;
    }
    /*
/SHG/TRUCKEE RIVER/TEMP-AIR/31JAN2016:2400//INTERPOLATED-ROUNDED/

= = = GridInfo = = =
GridInfo Version:  1
Grid Type: ALBERS (420)
Info Size: 164
Grid Info Size: 124
Start Time(assumed UTC): 1 February 2016, 00:00
End Time(assumed UTC):   31 January 2016, 24:00
Data Units: Deg C
Data Type: INST-VAL (2)
Lower Left CellX: -1042
Lower Left CellY: 991
Number Of CellsX: 75
Number Of CellsY: 75
Cell Size: 2000.0
Compression Method: ZLIB Deflate (26)
Size of Compressed Elements: 2091
Compression Scale Factor: 1.0
Compression Base: 0.0
Max Data Value: -2.0
Min Data Value: -15.1
Mean Data Value: -6.5389285
Number Of Ranges: 13
= = = = = = = = Range Limit Table = = = = = = = =
Range		> or =		Incremental Count
NULL		5625		4469
-40.0		1156		0
-30.0		1156		0
-20.0		1156		1
-15.0		1155		96
-10.0		1059		754
-5.0		305		305
0.0		0		0
====================================================

= = = AlbersInfo = = =
Standard Hydrologic Grid Spatial Reference
Projection Datum: NAD 83 (2)
Projection Units: METERS
First Standard Parallel: 29.5
Second Standard Parallel: 45.5
Central Meridian: -96.0
Latitude Of ProjectionOrigin: 23.0
False Easting: 0.0
False Northing: 0.0
XCoord Of Grid Cell Zero: 0.0
YCoord Of Grid Cell Zero: 0.0
=========================================================         
     */
    [TestMethod]
    public void BasicReadWrite()
    {
      string path = "/SHG/TRUCKEE RIVER/TEMP-AIR/31JAN2016:2400//INTERPOLATED-ROUNDED/";
      string fn = TestUtility.GetCopyForTesting("version7AlbersGrid.dss");

      Grid g1 = null;
      using (DssReader r = new DssReader(fn)) 
      {
        g1 = r.GetGrid(path, true);
      }


      //string s = g2.Info();

    }

    [TestMethod]
    public void V7ReadSimpleAlbersUndefinedProjection()
    {
      string path = "/a/b/c/01jan2001:1200/01jan2001:1300/f/";
      string fn = TestUtility.GetCopyForTesting("gridtest.dss");

      Grid g1 = null;
      Grid g2 = null;
      using (DssReader r = new DssReader(fn))
      {

        g1 = r.GetGrid(path, false);
        g2 = r.GetGrid(path, true);
      }
      Assert.IsNotNull(g1);
      Assert.IsNotNull(g2);

      var x = g2.Data;
      for (int i = 0; i < x.Length; i++)
      {
        float expected = i * 1.2f;
        Assert.AreEqual(expected, x[i], 0.0001);
      }
      float delta = 0.000001f; // tolarance for comparisons;
      Assert.AreEqual(DssDataType.PER_AVER, g2.DataType); //  Data Type: PER-AVER
      Assert.AreEqual(50, g2.Data.Length);
      Assert.AreEqual(0, g2.LowerLeftCellX); //  Lower Left Cell: (0, 0)
      Assert.AreEqual(0, g2.LowerLeftCellY);
      Assert.AreEqual("mm", g2.DataUnits.ToLower()); // Data Units: mm
      Assert.AreEqual(5, g2.NumberOfCellsX); //Grid Extents: (5, 10)
      Assert.AreEqual(10, g2.NumberOfCellsY);
      Assert.AreEqual(5.0f, g2.CellSize, delta); //Cell Size: 5.0

      Assert.AreEqual(60.0f, g2.MaxDataValue, delta); // Max Data Value: 60.0
      Assert.AreEqual(0.001, g2.MinDataValue, delta); //Min Data Value: 0.001
      Assert.AreEqual(30.0f, g2.MeanDataValue, delta); // Mean Data Value: 30.0
      Assert.AreEqual(10.2f, g2.XCoordOfGridCellZero, delta);// XCoord Of Grid Cell Zero: 10.2
      Assert.AreEqual(20.3f, g2.YCoordOfGridCellZero, delta); //YCoord Of Grid Cell Zero: 20.3

      Assert.AreEqual(path.ToLower(), g2.PathName.ToLower());
      Assert.AreEqual(GridType.ALBERS, g2.GridType); //Grid Type: ALBERS
      Assert.AreEqual(12, g2.NumberOfRanges);

      /*

      Start Time(assumed UTC): 1 January 2001, 12:00
      End Time(assumed UTC):   1 January 2001, 13:00
       = = = AlbersInfo = = =
      Projection Datum: UNDEFINED DATUM
      Projection Units: 
      First Standard Parallel: 0.0
      Second Standard Parallel: 0.0
      Central Meridian: 0.0
      Latitude Of ProjectionOrigin: 0.0
      False Easting: 0.0
      False Northing: 0.0
      */
    }

    /*
/SHG/CONNECTICUT/AIRTEMP/07MAY2019:1400//GAGEINTERP/

= = = GridInfo = = =
GridInfo Version:  1
Grid Type: ALBERS (420)
Info Size: 164
Grid Info Size: 124
Start Time(assumed UTC): 7 May 2019, 14:00
End Time(assumed UTC):   30 December 1899, 24:00
Data Units: DEG F
Data Type: INST-VAL (2)
Lower Left CellX: 862
Lower Left CellY: 1117
Number Of CellsX: 185
Number Of CellsY: 260
Cell Size: 2000.0
Compression Method: PRECIP_2_BYTE (101001)
Size of Compressed Elements: 18466
Compression Scale Factor: 100.0
Compression Base: 0.0
Max Data Value: 71.00885
Min Data Value: 31.575598
Mean Data Value: 62.9296
Number Of Ranges: 13
= = = = = = = = Range Limit Table = = = = = = = =
Range		> or =		Incremental Count
NULL		48100		39106
-500.0		8994		0
-50.0		8994		0
-10.0		8994		0
0.0		8994		0
10.0		8994		0
30.0		8994		15
40.0		8979		98
50.0		8881		8817
70.0		64		64
90.0		0		0
====================================================

= = = AlbersInfo = = =
Standard Hydrologic Grid Spatial Reference
Projection Datum: NAD 83 (2)
Projection Units: METERS
First Standard Parallel: 29.5
Second Standard Parallel: 45.5
Central Meridian: -96.0
Latitude Of ProjectionOrigin: 23.0
False Easting: 0.0
False Northing: 0.0
XCoord Of Grid Cell Zero: 0.0
YCoord Of Grid Cell Zero: 0.0
=========================================================         
    */
    [TestMethod]
    public void ReadConnecticutGrids()
    {
      // This path doesn't have a  E part.....
      DssPath path = new DssPath("/SHG/CONNECTICUT/AIRTEMP/07MAY2019:1400//GAGEINTERP/");
      string fn = TestUtility.BasePath + "ConnecticutGrids.dss";

      DssReader r = new DssReader(fn);

      var catalog = r.GetCatalog();

      for (int i = 0; i < catalog.Count; i++)
      {
        var p = catalog[i].FullPath;
        if (p.IndexOf("GAGEINTERP") > 0)
          Console.WriteLine(p);
      }

      Assert.IsTrue(r.ExactPathExists(path));

      Grid g1 = r.GetGrid(path, false);
      Grid g2 = r.GetGrid(path, true);
      Assert.IsNotNull(g1);
      Assert.IsNotNull(g2);

      string expected = Grid.SHG_SRC_DEFINITION;

      Console.WriteLine("expected:\n" + expected);
      Console.WriteLine(g2.SRSDefinition);
      Assert.IsTrue(expected.ToLower() == g2.SRSDefinition.ToLower());
      // Assert.IsTrue(g1.SRSDefinitionType == 1);

    }

    [TestMethod]
    public void ReadAllGrids()
    {
      string fn = TestUtility.BasePath + "precip.2018.09.dss";
      string pathname = "/SHG/MARFC/PRECIP///NEXRAD/";
      var targetDSSPath = new DssPath(pathname);

      double sum = 0;
      using (var dssr = new DssReader(fn))
      {
        var cat = dssr.GetCatalog(false);
        var paths = cat.Paths.Select(dssPath => dssPath.FullPath).ToArray();

        var filterPaths = PathAssist.FilterByPart(paths, targetDSSPath);

        for (int i = 0; i < filterPaths.Count; i++)
        {
          string subpath = filterPaths[i];
          var dssPath = new DssPath(subpath);

          var grd = dssr.GetGrid(dssPath, true);
          sum += grd.MaxDataValue;
        }

        Console.WriteLine(sum.ToString());
      }
    }

    [TestMethod]
    public void FilterGridByParts()
    {
      string fn = TestUtility.BasePath + "NDFD_Wind.dss";
      using (DssReader dssr = new DssReader(fn))
      {
        var cat = dssr.GetCatalog(false);
        var paths = cat.Paths.Select(dssPath => dssPath.FullPath).ToArray();

        string _pathname = "/SHG/CONUS/WIND DIRECTION///NDFD (YUBZ98) (001-003)/";
        var targetDSSPath = new DssPath(_pathname);
        var filterPaths = PathAssist.FilterByPart(paths, targetDSSPath);
      }
    }

  }
  
}
