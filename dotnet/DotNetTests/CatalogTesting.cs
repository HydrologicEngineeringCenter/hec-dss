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
    public void GridTest7()
    {
      var fn = Path.Combine(TestUtility.BasePath, "SixAndOneHour.dss");
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
    public void GridTest6()
    {
      var fn = Path.Combine(TestUtility.BasePath, "SixAndOneHour6.dss");
      using (DssReader reader = new DssReader(fn))
      {
        var cat = reader.GetCatalog();
        var tbl = cat.ToDataTable();
        Console.WriteLine(cat);
        Assert.AreEqual(29, cat.CondensedPaths.Count);
        Assert.AreEqual(29, cat.Count);


      
      }

    }

    [TestMethod]
    public void DssPathBug()
    {
      var p = new DssPath("A", "B", "C", "D", "E", "F");
      var s = p.FullPath;

     }


    ////SACRAMENTO/PRECIP-INC/01Jan1877/1Day/OBS/

    [TestMethod]
    public void SortedCatalog()
    {
      string filename = "test_sorted_catalog.dss";
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
      w.Write(ts, true);

    }


    [TestMethod]
    public void MissingPartF()
    {
     // var filename = @"C:\project\dotnet\Output\x64\Debug\dot_net_ReadWriteDailyRegular.dss";
     string filename = TestUtility.BasePath + "sample7.dss";
      DataTable rval = new DataTable();
      using (DssReader r = new DssReader(filename))
      {
        var catalog = r.GetCatalog(true);
        rval = catalog.ToDataTable();

      }
    }
    [TestMethod]
    public void CondencedCatalog7()
    {
      var tbl = Catalog(TestUtility.BasePath + "sample7.dss");
      Assert.IsTrue(tbl.Rows.Count > 30);

    }

    [TestMethod]
    public void CondencedCatalog7a()
    {
      var r = new DssWriter(TestUtility.BasePath + "sample7a.dss");
      var c = r.GetCatalog(true);

    }

    [TestMethod]
    public void CondencedCatalog7Extended()
    {
      var tbl = Catalog(TestUtility.BasePath + "sample7_units_xyz.dss", true);
      Assert.IsTrue(tbl.Rows.Count > 30);

      var s = "A='' and B='SACRAMENTO' and C = 'PRECIP-INC' and E='1Day' and F= 'OBS' and XOrdinate='10'";
      Assert.AreEqual(1, tbl.Select(s).Length);

      s = "A='TEST' and B='COWLITZ' and C = 'FREQ-FLOW' and D='EXAMPLE' and F= '' and RecordType='PairedData' and XOrdinate='-123' and YOrdinate='-11151965' and units ='cfs'";
      Assert.AreEqual(1, tbl.Select(s).Length);

    }


    private static DataTable Catalog(string filename, bool extendedInfo = false)
    {
      DataTable rval = new DataTable();
      using (DssReader r = new DssReader(filename))
      {
        Console.WriteLine(Directory.GetCurrentDirectory());
        Console.WriteLine(System.IO.Path.GetFileName(filename));

        var catalog = r.GetCatalog(extendedInfo);
        rval = catalog.ToDataTable();

        //PrintDataTable(rval);
        //foreach (var item in catalog)
        //{
        //  Console.WriteLine(item.FullPath + ",  " + item.RecordTypeName);
        //}
        //Console.WriteLine(catalog.Count);
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
    [Ignore]
    public void CondencedCatalog6()
    {
     var t = Catalog(TestUtility.BasePath + "sample6.dss", true);
      Console.WriteLine("sample6.dss catalog has "+t.Rows.Count+" rows ");
      Assert.IsTrue(t.Rows.Count == 36);
    }

  }
}
