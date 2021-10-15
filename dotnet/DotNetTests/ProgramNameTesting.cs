using Hec.Dss;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using static Hec.Dss.Quality;

namespace DSSUnitTests
{
    /// <summary>
    /// Summary description for ProgramNameTesting
    /// </summary>
    [TestClass]
    public class ProgramNameTesting
    {
        public ProgramNameTesting()
        {
            //
            // TODO: Add constructor logic here
            //
        }

        [TestMethod]
        public void SetProgramName_Dss6()
        {
            string orig = TestUtility.BasePath + "simpleQCT6_RTS.dss";
            string fn = TestUtility.BasePath + "simpleQCT6Copy_RTS.dss";
            File.Delete(fn);
            File.Copy(orig, fn);
            
            var ts = TimeSeriesTest.CreateSampleTimeSeries(new DateTime(2020, 1, 2), "cfs", "INST", size: 10);
            ts.Path = new DssPath("A", "B", "C", "01JAN2020", E: "1DAY", F: "F");
            using (DssWriter w = new DssWriter(fn))
            {
                DssGlobals.SetProgramName("DOTNET");
                w.Write(ts);
            }

            using (DssReader r = new DssReader(fn))
            {
                var temp = r.GetTimeSeries(ts.Path);
                Assert.AreEqual("DOTNET", temp.ProgramName);
            }
        }

        [TestMethod]
        public void SetProgramNameForMultipleRecords_Dss6()
        {
            string orig = TestUtility.BasePath + "simpleQCT6_RTS.dss";
            string fn = TestUtility.BasePath + "simpleQCT6Copy_RTS.dss";
            File.Delete(fn);
            File.Copy(orig, fn);
            List<DssPath> paths = new List<DssPath>();
            using (DssWriter w = new DssWriter(fn))
            {
                DssGlobals.SetProgramName("DOTNET");
                for (int i = 0; i < 10; i++)
                {
                    var ts = TimeSeriesTest.CreateSampleTimeSeries(new DateTime(2020, 1, 2), "cfs", "INST", size: 10);
                    ts.Path = new DssPath("a" + i.ToString(), "b", "c", "01Jan2020", E: "1Day", F: "f");
                    paths.Add(ts.Path);
                    w.Write(ts);
                }
            }

            using (DssReader r = new DssReader(fn))
            {
                foreach (DssPath path in paths)
                {
                    var ts = r.GetTimeSeries(path);
                    Assert.AreEqual("DOTNET", ts.ProgramName);
                }
            }
        }

        [TestMethod]
        public void SetProgramName_Dss7()
        {
            string fn = TestUtility.BasePath + "simpleQCT7.dss";
            File.Delete(fn);
            
            var ts = TimeSeriesTest.CreateSampleTimeSeries(new DateTime(2020, 1, 2), "cfs", "INST", size: 10);
            ts.Path = new DssPath("a", "b", "c", "01Jan2020", E: "1Day", F: "f");
            using (DssWriter w = new DssWriter(fn))
            {
                DssGlobals.SetProgramName("dotnet");
                w.Write(ts);
            }

            using (DssReader r = new DssReader(fn))
            {
                var temp = r.GetTimeSeries(ts.Path);
                Assert.AreEqual("dotnet", temp.ProgramName);
            }
        }

        [TestMethod]
        public void SetProgramNameForMultipleRecords_Dss7()
        {
            string fn = TestUtility.BasePath + "simpleQCT7.dss";
            File.Delete(fn);
            List<DssPath> paths = new List<DssPath>();
            using (DssWriter w = new DssWriter(fn))
            {
                DssGlobals.SetProgramName("dotnet-test");
                for (int i = 0; i < 10; i++)
                {
                    var ts = TimeSeriesTest.CreateSampleTimeSeries(new DateTime(2020, 1, 2), "cfs", "INST", size: 10);
                    ts.Path = new DssPath("a" + i.ToString(), "b", "c", "01Jan2020", E: "1Day", F: "f");
                    paths.Add(ts.Path);
                    w.Write(ts);
                }
            }

            using (DssReader r = new DssReader(fn))
            {
                foreach (DssPath path in paths)
                {
                    var ts = r.GetTimeSeries(path);
                    Assert.AreEqual("dotnet-test", ts.ProgramName);
                }
            }
        }

        [TestMethod]
        public void SetProgramNameAndSaveAsFloat_Dss6()
        {
            string orig = TestUtility.BasePath + "simpleQCT6_RTS.dss";
            string fn = TestUtility.BasePath + "simpleQCT6Copy_RTS.dss";
            File.Delete(fn);
            File.Copy(orig, fn);

            var ts = TimeSeriesTest.CreateSampleTimeSeries(new DateTime(2020, 1, 2), "cfs", "INST", size: 10);
            ts.Path = new DssPath("A", "B", "C", "01JAN2020", E: "1DAY", F: "F");
            using (DssWriter w = new DssWriter(fn))
            {
                DssGlobals.SetProgramName("DOTNET");
                w.Write(ts, true);
            }

            using (DssReader r = new DssReader(fn))
            {
                var temp = r.GetTimeSeries(ts.Path);
                Assert.AreEqual("DOTNET", temp.ProgramName);
            }
        }

        [TestMethod]
        public void SetProgramNameAndSaveAsFloat_Dss7()
        {
            string fn = TestUtility.BasePath + "simpleQCT7.dss";
            File.Delete(fn);

            var ts = TimeSeriesTest.CreateSampleTimeSeries(new DateTime(2020, 1, 2), "cfs", "INST", size: 10);
            ts.Path = new DssPath("a", "b", "c", "01Jan2020", E: "1Day", F: "f");
            using (DssWriter w = new DssWriter(fn))
            {
                DssGlobals.SetProgramName("dotnet-test");
                w.Write(ts, true);
            }

            using (DssReader r = new DssReader(fn))
            {
                var temp = r.GetTimeSeries(ts.Path);
                Assert.AreEqual("dotnet-test", temp.ProgramName);
            }
        }
    }
}
