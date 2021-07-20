using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using DSSIO;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace DSSUnitTests
{
    //[TestClass]
    public class EnsembleTest
    {

        [TestMethod]
        public void V7SimpleTest()
        {
            string fn = "ensemble__double_V7SimpleTest.dss";
            int count = SaveEnsembles(fn, false, 7);

            ReadAndCheckCondencedCatalog(fn, count);

        }

        [TestMethod]
        public void V6SimpleTest()
        {
            string fn = "ensemble__double_V6SimpleTest.dss";
            int count = SaveEnsembles(fn, false,6);

            ReadAndCheckCondencedCatalog(fn, count);

        }

        [TestMethod]
        public void V6Issue()
        {
            string fn = "ensemble_double_v6.dss";
            int count = SaveEnsembles(fn, false, 6);
            ReadAndCheckCondencedCatalog(fn, count);
        }


        private static void ReadAndCheckCondencedCatalog(string fn, int expectedCount)
        {
            using (var r = new DSSReader(fn))
            {
                Stopwatch sw = Stopwatch.StartNew();
                var paths = r.GetCondensedPathNames();
                sw.Stop();

                //DSSPathCollection.WriteToFile(paths, @"c:\temp\v6catalog.csv");
                Console.WriteLine("generated catalog in " + sw.Elapsed.TotalSeconds.ToString("F2"));
                Assert.AreEqual(expectedCount, paths.Count, " count of condenced path should be the same as count created");


            }
        }

        private static int SaveEnsembles(string fn, bool saveAsFloat, int version)
        {

            HEFS_Reader.Implementations.TimeSeriesOfEnsembles e = new HEFS_Reader.Implementations.TimeSeriesOfEnsembles();
            //DateTime startTime = DateTime.Now.Date.AddDays(-4).AddHours(12);
            DateTime startTime =  new DateTime(2013, 11, 1, 12, 0, 0);
            DateTime endTime = DateTime.Now.Date.AddDays(-1).AddHours(12);
            var watershed = HEFS_Reader.Enumerations.Watersheds.RussianNapa;
            IList<IList<HEFS_Reader.Interfaces.IEnsemble>> ensembles = e.getDataForWatershedAndTimeRange(watershed, startTime, endTime);

            int numEnsembles = ensembles.Count;
            int numLocations = ensembles[0].Count;
        
             
            File.Delete(fn);
            Hec.Dss.DSS.ZSet("DSSV", "", version);

            int count = 0;
            using (var w = new DSSWriter(fn))
            {
                for (int loc = 0; loc < numLocations; loc++) // location
                {
                     
                    for (int ts = 0; ts< numEnsembles; ts++) // number of forecasts
                    {
                        var ensembleMembers = ensembles[ts][loc].getMembers();
                            for (int ensembleMember = 1; ensembleMember < ensembleMembers.Count; ensembleMember++)
                            { ///   A/B/FLOW//1 Hour/<FPART></FPART>
                            //// c:  ensemble.p
                            var ensemble = ensembleMembers[ensembleMember];
                            var t= ensembles[ts][loc].getIssueDate();
                            //  /RUSSIANNAPA/APCC1/FLOW/01SEP2019/1HOUR/C:000002|T:0212019/
                             string F = "C:" + ensembleMember.ToString().PadLeft(6, '0') + "|T:" +
                                   t.Day.ToString().PadLeft(3, '0') + t.Year.ToString();

                            var path = "/"+watershed.ToString() + "/" + ensembles[ts][loc].getLocationName() +"/Flow//1Hour/" + F + "/";
                            DSSTimeSeries timeseries = new DSSTimeSeries
                            {
                                Values = Array.ConvertAll(ensemble.getValues(), item => (double)item), 
                                Units = "",
                                DataType = "INST-VAL",
                                Path = path,
                                StartDateTime = ensemble.getTimes()[0]
                                };

                                w.Write(timeseries, saveAsFloat);
                                count++;
                                //                            int status = w.StoreTimeSeriesRegular(path, d, 0, DateTime.Now.Date, "cfs", "INST-VAL");

                            }
                    }
                }
                FileInfo fi = new FileInfo(fn);
                var s = BytesToString(fi.Length);
                Console.WriteLine(fn);
                Console.WriteLine(s);
                Console.WriteLine("Count = " + count);
            }
            return count;
        }

        private static int SaveEnsemblesOld(string fn, bool saveAsFloat, int version,int numEnsembles=50,int numYears=50, int numDays=365)
        {
            File.Delete(fn);
            Hec.Dss.DSS.ZSet("DSSV", "", version);

            int count = 0;
            using (var w = new DSSWriter(fn))
            {
                for (int loc = 0; loc < 1; loc++)
                {
                    for (int year = 2000; year < 2000 + numYears; year++)
                    {
                        DateTime t = new DateTime(year, 1, 1); 
                        for (int day = 1; day <= numDays; day++)
                        {

                            for (int ensemble = 1; ensemble <= numEnsembles; ensemble++)
                            { ///   A/B/FLOW//1 Hour/<FPART></FPART>
                                //// c:  ensemble.p

                                string F = "C:" + ensemble.ToString().PadLeft(6, '0') + "|T:" +
                                    day.ToString().PadLeft(3, '0') + year.ToString();

                                var path = "/A/B/Flow//1Hour/" + F + "/";
                                DSSTimeSeries ts = new DSSTimeSeries
                                {
                                    Values = CreateDoubles(14 * 24),
                                    Units = "cfs",
                                    DataType = "INST-VAL",
                                    Path = path,
                                    StartDateTime = t.AddHours(1)

                                };

                                w.Write(ts, saveAsFloat);
                                count++;
                                //                            int status = w.StoreTimeSeriesRegular(path, d, 0, DateTime.Now.Date, "cfs", "INST-VAL");

                            }
                            t = t.AddDays(1);
                        }

                    }
                }
                FileInfo fi = new FileInfo(fn);
                var s = BytesToString(fi.Length);
                Console.WriteLine(fn);
                Console.WriteLine(s);
                Console.WriteLine("Count = "+count);
            }
            return count;
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
