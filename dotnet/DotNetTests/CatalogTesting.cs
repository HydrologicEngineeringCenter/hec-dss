using System;
using Hec.Dss;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Data;
using System.IO;

namespace DSSUnitTests
{
  [TestClass]
  public class CatalogTesting
  {
    [TestMethod]
    public void GetCatalogAllTypesWithMetaData()
    {
      var fn = TestUtility.GetCopyForTesting("examples-all-data-types.dss");
      using (DssReader reader = new DssReader(fn))
      {
        var cat = reader.GetCatalog();

        var tbl = cat.ToDataTable();
        Console.WriteLine(tbl);
      }

    }


      [TestMethod]
    public void CatalogWith_6Hour_and_1Hour_Grids()
    {
      var fn = TestUtility.GetCopyForTesting("SixAndOneHour.dss");
      using (DssReader reader = new DssReader(fn))
      {
        var cat = reader.GetCatalog();
        Assert.AreEqual(29,cat.CondensedPaths.Count);
        Assert.AreEqual(29, cat.Count);


        var tbl = cat.ToDataTable();
        Console.WriteLine(cat);
      }

    }

    [TestMethod]
    public void DssPathBug()
    {
      var p = new DssPath("A", "B", "C", "D", "E", "F");
      var s = p.FullPath;

     }


    [TestMethod]
    public void SortedCatalog()
    {
      string filename = TestUtility.GetSimpleTempFileName(".dss");
      File.Delete(filename);
      using (DssWriter w = new DssWriter(filename))
      {
        TimeSeries s = new TimeSeries();


        ///YYYYMMDD-hhmm
        string[] unsorted = new string[] {
"/Z/csharp/values/31Dec2000/1Day//",
"/A/csharp/values/31Dec2000/1Day/C:000001|T:20130312-1201|/",
"/B/csharp/values/31Dec2000/1Day//",
"/A/csharp/values/31Dec2000/1Day/C:000001|T:20130311-1201|/",
"/B/csharp/values/01Jan2000/1Day/C:000002|T:20130312-1201|/"
        };
        for (int i = 0; i < unsorted.Length; i++)
        {
          DateTime t = new DateTime(2000, 12, 31);
          if (i == 4)
            t = new DateTime(2000, 1, 1);
          AddTS(w, t, unsorted[i]);
        }

        string[] sorted = new string[] {
"/A/csharp/values/31Dec2000/1Day/C:000001|T:20130311-1201|/",
"/A/csharp/values/31Dec2000/1Day/C:000001|T:20130312-1201|/",
"/B/csharp/values/01Jan2000/1Day/C:000002|T:20130312-1201|/",
"/B/csharp/values/31Dec2000/1Day//",
"/Z/csharp/values/31Dec2000/1Day//"
        };

        var paths = w.GetCatalog();
        foreach (var path in paths)
        {
          Console.WriteLine(path.FullPath);
        }
        Console.WriteLine();

        for (int i = 0; i < paths.Count; i++)
        {
          Console.WriteLine(paths[i].PathWithoutDate);
          var p = new DssPath(sorted[i]);
          Assert.AreEqual(p.PathWithoutDate, paths[i].PathWithoutDate);
        }

      }

    }

    private void AddTS(DssWriter w, DateTime t, string path)
    {
      var ts = TimeSeriesTest.CreateSampleTimeSeries(t, "ft", "", 86400, 11);
      ts.Path = new DssPath(path);
      ts.LocationInformation = null;
      w.Write(ts, true);

    }


    [TestMethod]
    public void MissingPartF()
    {
     string filename = TestUtility.GetCopyForTesting( "sample7.dss");
      DataTable rval = new DataTable();
      using (DssReader r = new DssReader(filename))
      {
        var catalog = r.GetCatalog();
        rval = catalog.ToDataTable();

      }
    }
    [TestMethod]
    public void CondencedCatalog7()
    {
      var tbl = Catalog(TestUtility.GetCopyForTesting("sample7.dss"));
      Assert.IsTrue(tbl.Rows.Count > 30);

    }

    [TestMethod]
    public void CondencedCatalog7a()
    {
      var r = new DssWriter(TestUtility.GetCopyForTesting("sample7a.dss"));
      var c = r.GetCatalog();
      Assert.AreEqual(38,c.Count);
    }


    private static DataTable Catalog(string filename)
    {
      DataTable rval = new DataTable();
      using (DssReader r = new DssReader(filename))
      {
        Console.WriteLine(Directory.GetCurrentDirectory());
        Console.WriteLine(System.IO.Path.GetFileName(filename));

        var catalog = r.GetCatalog();
        rval = catalog.ToDataTable();

      }
      return rval;
    }

    private static void PrintDataTable(DataTable table)
    {
      for (int i = 0; i < table.Columns.Count; i++)
      {
        Console.Write(table.Columns[i].ColumnName);
        if (i == table.Columns.Count - 1)
          Console.WriteLine();
        else
          Console.Write(",");
      }
      DataView view = table.DefaultView;
      for (int r = 0; r < table.Rows.Count; r++)
      {
        for (int c = 0; c < table.Columns.Count; c++)
        {
          if (table.Columns[c].DataType.ToString() == "System.String")
          {
            Console.Write("\"" + view[r][c] + "\"");
          }
          else
          {
            Console.Write(view[r][c]);
          }
          if (c < table.Columns.Count - 1)
          {
            Console.Write(",");
          }
        }
        Console.WriteLine();
      }
    }

    [TestMethod]
    public void FindExactPath()
    {
      string dssFile = TestUtility.GetCopyForTesting(@"benchmarks6/BaldEDmbrk7.dss");
      string path = @"/BALD EAGLE LOC HAV/105178.6/FLOW-CUM/17FEB1999-23FEB1999/1Minute/DAMBRKSIMBRCH/";
      using (DssReader dss = new DssReader(dssFile))
      {
        DssPathCollection paths = dss.GetCatalog();
        var dsspath = paths.FindExactPath(path);
        var ts = dss.GetEmptyTimeSeries(dsspath);
        Assert.AreEqual("ACRE-FT", ts.Units);
        Assert.AreEqual("INST-CUM", ts.DataType);
      }
    }

    [TestMethod]
    public void FilterByParts()
    {
      var dssFile = TestUtility.GetCopyForTesting("version7AlbersGridsTimeSeries.dss");

      using DssWriter dss = new DssWriter(dssFile);
      DssPathCollection paths = dss.GetCatalog();

      Assert.AreEqual(5, paths.FilterByPart(bPart: "BERLIN").Count);
      Assert.AreEqual(1, paths.FilterByPart(bPart: "BERLIN", cPart: "FLOW-LOCAL").Count);
    }
  }
}
