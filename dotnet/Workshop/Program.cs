using Hec.Dss;
using System;
using System.Diagnostics;
using System.IO;

namespace Workshop
{
  class Program
    {
        static void Main(string[] args)
    {
      ReadBigCatalog();
      //ReadEnsembles();
     // GetEmptyTimeSeries();

    }
    static void ReadBigCatalog()
    {
      String fileName = @"C:\project\dss-file-collection\FRM_UMR_Model.p01\FRM_UMR_Model.dss";

      if (File.Exists(fileName) == false)
      {
        Console.WriteLine("File not found: " + fileName);
        Console.WriteLine("Skipping ReadBigCatalog test.");
        return;
      }
      using (DssReader r = new DssReader(fileName))
      {
        var catalog = r.GetCatalog(); // 2.5 minutes
        //var catalog = r.GetCatalog(true);  // 64.2 min
      }

    }

    private static void GetEmptyTimeSeries()
    {
      //ReadEnsembles();
      string fn = @"c:\temp\sample6.dss";
      using (DssReader r = new DssReader(fn, DssReader.MethodID.MESS_METHOD_GLOBAL_ID, DssReader.LevelID.MESS_LEVEL_CRITICAL))
      {
        //var paths = r.GetCatalog();

        var ts = r.GetEmptyTimeSeries(new DssPath("//SACRAMENTO/PRECIP-INC/01Jan1877/1Day/OBS/"));
        // var ts = r.GetTimeSeries(new DSSPath("//SACRAMENTO/PRECIP-INC/01Jan1877/1Day/OBS/"));

      }
    }

    private static void ReadEnsembles()
    {
      var sw = Stopwatch.StartNew();
      var fn = @"c:\temp\ensemble_V71000.dss";
      int count = 0;
      // initial 6 sec
      //
      using (DssReader r = new DssReader(fn, DssReader.MethodID.MESS_METHOD_GLOBAL_ID, DssReader.LevelID.MESS_LEVEL_CRITICAL))
      {
        var paths = r.GetCatalog();
        foreach (DssPath item in paths)
        {
          var s = r.GetTimeSeries(new DssPath(item.PathWithoutDate));
          count++;
          if (count % 100 == 0)
            Console.Write(".");
          //         if (count > 400)
          //         break;
        }

      }
      Console.WriteLine();
      sw.Stop();
      Console.WriteLine(sw.Elapsed.TotalSeconds.ToString("F2"));
    }
  }
}
