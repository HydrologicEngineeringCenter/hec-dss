using DSSUnitTests;
using Hec.Dss;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DSSUnitTests
{
   [TestClass]
   public class SqueezeTesting
   {

      /// <summary>
      /// 
      /// </summary>
      [TestMethod]
      public void Squeezev7()
      {
         String fn = TestUtility.GetSimpleTempFileName(".dss");
         Console.WriteLine(fn);
         
         int dssVersion = 7;
         //DssGlobals.SetDefaultVersion(version);
         using (DssWriter r = new DssWriter(fn, dssVersion))
         {
            // create 100 time series.
            for (int i = 0; i < 100; i++)
            {
               string tag = i <50? "delete": "keeper";
               var path = "//river" + i + "/flow//1Day/"+tag+"/";
               var ts = CreateData(path, 100);
               r.Write(ts);
            }
            // delete all time series that have Fpart = 'delete'
            var catalog = r.GetCatalog().UnCondensedPaths;
            foreach (var item in catalog)
            {
               if (item.Fpart.ToLower() == "delete")
                  
                  r.DeleteRecord(item.FullPath);
            }
         }
         long sizeBefore = new System.IO.FileInfo(fn).Length / 1000;
         Assert.IsTrue(sizeBefore > 600,"expected size about 714Kb");
         // before squeeze file is 714 Kb,  after squeeze file is about 361 Kb
         DssWriter.Squeeze(fn); 
         // Hec.Dss.Native.DSS.ZSqueeze(fn); //hack 
         long sizeAfter = new System.IO.FileInfo(fn).Length / 1000;
         Assert.IsTrue(sizeAfter < 400,"Expected size about 361 Kb");
         
      }

      private static TimeSeries CreateData(string path,int size)
      {
         double[] data = new double[size];
         DateTime[] dateTimes = new DateTime[size];
         DateTime t = new DateTime(2022, 1, 1);
         for (int i = 0; i < size; i++)
         {
            data[i] = i * 10.0;
            dateTimes[i] = t;
            t = t.AddDays(1);
         }
         TimeSeries ts = new TimeSeries(path, data,dateTimes[0], "cfs", "PER-AVE");
         
         return ts;
      }
   }
}
