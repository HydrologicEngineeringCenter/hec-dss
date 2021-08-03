using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Running;
using DSSIO;
using static DotNetBenchmarks.DSSIOBenchmarks;

namespace DotNetBenchmarks
{
  class Program
  {
    static void Main(string[] args)
    {
      //for running in debug, uncomment.
      //BenchmarkSwitcher.FromAssembly(typeof(Program).Assembly).Run(args, new DebugInProcessConfig());

      //var summary = BenchmarkRunner.Run<UniqueAPartTests>();
      //var summary = BenchmarkRunner.Run<CondensePaths>();

      //var summary = BenchmarkRunner.Run<Interning>();
      //GC.Collect();
      //var memoryBefore = GC.GetTotalMemory(false);
      //using (DSSReader reader  = new DSSReader(@"M:\_Projects\HEC-DSS\test-data\benchmarks6\gavunstdy.dss"))
      //{
      //  var x = reader.GetCondensedPathNames();
      //  GC.Collect();
      //  var memoryAfter = GC.GetTotalMemory(false);
      //  Console.WriteLine(x.Count + " : " + memoryBefore + "," + memoryAfter);
      //}

      string fn = @"C:\Temp\DSS Test\ensemble_V6_100.dss";
      // string fn = @"C:\Temp\SampleDSSFiles\ensemble_V7_10.dss";

      ReadDataset(fn);
    }

    public static void ReadDataset(string filename)
    {
      // Testing - remove safety checks
      //DSSReader.UseTrainingWheels = false;
      DSSReader.UseTrainingWheels = false;
      using (var reader = new DSSReader(filename, DSSReader.MethodID.MESS_METHOD_GENERAL_ID, DSSReader.LevelID.MESS_LEVEL_NONE))
      {
        var dssPaths = reader.GetCatalog(); // sorted
        //var pc = new PathComparer();
        //var dssPaths = rawDssPaths.OrderBy(a => a, pc).ToArray(); // sorted
        int size = dssPaths.Count;
        if (size == 0)
        {
          throw new Exception("Empty DSS catalog");
        }

        // /RUSSIANNAPA/APCC1/FLOW/01SEP2019/1HOUR/C:000002|T:0212019/
        for (int i = 0; i < size; i++)
        {
          if (i % 100 == 0)
            Console.Write(".");

          DSSPath path = dssPaths[i];
          //string location = path.Bpart;
          //int memberidx = int.Parse(path.Fpart.Split('|')[0].Split(':').Last().TrimStart('0'));

          // DateTime issueDate = ParseIssueDate(path.Fpart);

          var ts = reader.GetTimeSeries(path);
        }
      }
    }



  }
}
