using System;
using System.Collections.Generic;
using System.IO;
using Hec.Dss;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace DSSUnitTests
{
    [TestClass]
    public class LimitsTest
    {
      [TestMethod]
      public void LargePartF()
      {
         string fn = "large_F_part.dss";
         File.Delete(fn);

         using (DssWriter w = new DssWriter(fn))
         {
            var F = 128.ToString().PadRight(128, 'a');
            DssPath path = new DssPath("/feature/increase-part-f/FLOW//1day/" + F + "/");

            var d = CreateDoubles(10);
            int status = w.Write(new TimeSeries(path, d, DateTime.Now.Date, "cfs", "INST-VAL"));

            if (status != 0)
               throw new Exception("error writing with F part ='" + F + "'");
            var s = w.GetTimeSeries(path);
            if (s == null || s.Count != 10)
               throw new Exception("error reading with F part ='" + F + "'");
         }
      }

        /// <summary>
        /// 7.99 GB file with 5000 series and 10^7 points each.
        /// </summary>
        [Ignore]
        [TestMethod]
        public void V6()
        {
            string fn = "crash_me6.dss";

            File.Delete(fn);
            Hec.Dss.Native.DSS.ZSet("DSSV", "", 6);

      using (DssWriter w = new DssWriter(fn))
      {
        for (int pn = 0; pn < 7000; pn++)
        {
          var d = CreateDoubles((int)Math.Pow(10, 7));

          string path = "/dss-test/csharp/series" + pn + "//1day/file-size-test/";
          TimeSeries ts = new TimeSeries(path, d, DateTime.Now.Date, "cfs", "INST-VAL");
          //int status = w.StoreTimeSeriesRegular(path, d, 0, DateTime.Now.Date, "cfs", "INST-VAL");
          w.Write(ts);

          if (pn % 10 == 0)
          {
            FileInfo fi = new FileInfo(fn);
            var s = pn + " " + BytesToString(fi.Length);
            Console.WriteLine(s);
          }
        }

      }
    }

    /// <summary>
    /// https://stackoverflow.com/questions/281640/how-do-i-get-a-human-readable-file-size-in-bytes-abbreviation-using-net
    /// </summary>
    /// <param name="byteCount"></param>
    /// <returns></returns>
    static String BytesToString(long byteCount)
    {
      string[] suf = { "B", "KB", "MB", "GB", "TB", "PB", "EB" }; //Longs run out around EB
      if (byteCount == 0)
        return "0" + suf[0];
      long bytes = Math.Abs(byteCount);
      int place = Convert.ToInt32(Math.Floor(Math.Log(bytes, 1024)));
      double num = Math.Round(bytes / Math.Pow(1024, place), 1);
      return (Math.Sign(byteCount) * num).ToString() + suf[place];
    }


    private static double[] CreateDoubles(int size)
    {
      double[] d = new double[size];
      for (int i = 0; i < size; i++)
      {
        d[i] = i;
      }

      return d;
    }
  }
}
