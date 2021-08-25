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
            using (DssWriter w = new DssWriter(fn))
            {
                var ts = TimeSeriesTest.CreateSampleTimeSeries(new DateTime(2020, 1, 1), "cfs", "INST", size: 10);
                ts.Path = new DssPath("a", "b", "c", "", E: "1Day", F: "f");
                w.SetProgramName("DOTNET");
                w.Write(ts);
            }

            using (DssReader r = new DssReader(fn))
            {
                Assert.AreEqual("DOTNET", r.GetProgramName());
            }
        }

        [TestMethod]
        public void SetProgramName_Dss7()
        {
            string fn = TestUtility.BasePath + "simpleQCT7.dss";
            File.Delete(fn);
            using (DssWriter w = new DssWriter(fn))
            {
                var ts = TimeSeriesTest.CreateSampleTimeSeries(new DateTime(2020, 1, 1), "cfs", "INST", size: 10);
                ts.Path = new DssPath("a", "b", "c", "", E: "1Day", F: "f");
                w.SetProgramName("dotnet");
                w.Write(ts);
            }

            using (DssReader r = new DssReader(fn))
            {
                Assert.AreEqual("dotnet", r.GetProgramName());
            }
        }
    }
}
