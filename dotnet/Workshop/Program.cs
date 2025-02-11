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
         //String fileName = @"C:\Users\ktarb\Desktop\FRM_UMR_Model.dss";
         String fileNameOut = @"C:\Users\ktarb\Desktop\FRM_UMR_Model_subset.dss";
         //GetFirst5000Records(fileName, fileNameOut);
         ReadBigCatalog(fileNameOut, 10);
      //ReadEnsembles();
     // GetEmptyTimeSeries();

    }

      static void GetFirst5000Records(String fileName, String fileNameOut)
      {


         if (File.Exists(fileName) == false)
         {
            Console.WriteLine("File not found: " + fileName);
            Console.WriteLine("Skipping ReadBigCatalog test.");
            return;
         }

         Random random = new Random(DateTime.Now.Second); 
         using (DssWriter w = new DssWriter(fileNameOut))
         {
            using (DssReader r = new DssReader(fileName))
            {
               var catalog = r.GetCatalog();
               for (int i = 0; i < 8000; i++)
               {

                  int index = random.Next(0, catalog.Count - 1);
                  Console.WriteLine(   "Random index ["+index+"]");
                  var item = catalog[index];

                  if (item.RecordType == RecordType.RegularTimeSeries || item.RecordType == RecordType.IrregularTimeSeries)
                  {
                     var ts = r.GetTimeSeries(item);
                     w.Write(ts);
                  }
                  else if( item.RecordType == RecordType.PairedData)
                  {
                     var pd = r.GetPairedData(item.FullPath);
                     w.Write(pd);
                  }
                  else 
                  {
                     Console.WriteLine(   item.RecordType);
                  }
               }
            }
         }

      }
      

      
    static void ReadBigCatalog(String fileName,int repeat)
    {

      if (File.Exists(fileName) == false)
      {
        Console.WriteLine("File not found: " + fileName);
        Console.WriteLine("Skipping ReadBigCatalog test.");
        return;
      }
         for (int i = 0; i < repeat; i++)
         {
            using (DssReader r = new DssReader(fileName))
            {

               //var catalog = r.GetCatalog(); // 2.5 minutes
               var catalog = r.GetCatalog(true);  // 64.2 min

            }
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
