using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using BenchmarkDotNet.Attributes;
using System.Threading;
namespace DotNetBenchmarks
{
  public class DemoParallel
  {
    float[] input;
    float[] output;

    ThreadLocal<float[]> getBuffer = new ThreadLocal<float[]>(() => new float[10000]);

    [Params(10000, 1000000)]
    public int Count { get; set; }

    public DemoParallel()
    {
    }

    [GlobalSetup]
    public void Init()
    {
      input = new float[Count];
      output = new float[Count];

      for (int i = 0; i < Count; i++)
      {
        input[i] = (float)i * 10.2f;
      }
    }

    [Benchmark(Baseline = true)]
    public void Compute()
    {
      for (int i = 0; i < Count; i++)
      {
        output[i] = (float)Math.Sqrt(input[i]);
      }
    }
    [Benchmark()]
    public void ComputeParallel()
    {
      Parallel.For(0, Count, x => output[x] = (float)Math.Sqrt(input[x]));

      var buffer = getBuffer.Value;

      buffer = new float[2222];
      getBuffer.Value = buffer;
    }


    [Benchmark()]
    public void ComputeParallel_InnerLoop()
    {
      int workPerThread = 1000;
      int nIters = Count / workPerThread;
      Parallel.For(0, nIters, x =>
      {
        int start = x * workPerThread;
        for (int i = 0; i < workPerThread; i++)
        {
          output[start + i] = (float)Math.Sqrt(input[start + i]);
        }
      });

    }

  }
}
