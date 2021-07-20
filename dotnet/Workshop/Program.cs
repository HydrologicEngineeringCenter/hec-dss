using Hec.Dss;
using System;
using System.Diagnostics;

namespace Workshop
{
  class Program
    {
        static void Main(string[] args)
    {
      ReadEnsembles();
     // GetEmptyTimeSeries();

    }

    private static void GetEmptyTimeSeries()
    {
      //ReadEnsembles();
      string fn = @"c:\temp\sample6.dss";
      using (Reader r = new Reader(fn, Reader.MethodID.MESS_METHOD_GLOBAL_ID, DSSReader.LevelID.MESS_LEVEL_CRITICAL))
      {
        //var paths = r.GetCatalog();

        var ts = r.GetEmptyTimeSeries(new Path("//SACRAMENTO/PRECIP-INC/01Jan1877/1Day/OBS/"));
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
      using (DSSReader r = new DSSReader(fn, DSSReader.MethodID.MESS_METHOD_GLOBAL_ID, DSSReader.LevelID.MESS_LEVEL_CRITICAL))
      {
        var paths = r.GetCatalog();
        foreach (DSSPath item in paths)
        {
          var s = r.GetTimeSeries(item.PathWithoutDate);
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
