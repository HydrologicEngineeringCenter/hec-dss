using System;
using System.Linq;
using System.Collections.Generic;
using BenchmarkDotNet.Attributes;
using DSSIO;
using Hec.Dss;

namespace DotNetBenchmarks
{
  public class DSSIOBenchmarks
  {
    public class APartComparer : IEqualityComparer<DSSPath>
    {
      public bool Equals(DSSPath x, DSSPath y)
      {
        return x.Apart.Equals(y.Apart);
      }

      public int GetHashCode(DSSPath obj)
      {
        return obj.Apart.GetHashCode();
      }
    }

    // GC measurement is opt-in: https://adamsitnik.com/the-new-Memory-Diagnoser/
    [MemoryDiagnoser]
    public class UniqueAPartTests
    {
      const int TotalPathCount = 100000;

      private List<DSSPath> _paths;

      public UniqueAPartTests()
      {
        _paths = StringSortHelpers.MakePathList(TotalPathCount);
      }

      [Benchmark(Baseline = true)]
      public List<string> BaseUniqueAParts()
      {
        // Copied implementation from DSSIO
        Dictionary<string, bool> map = new Dictionary<string, bool>();
        List<string> unique = new List<string>();
        foreach (DSSPath path in _paths)
        {
          if (!map.ContainsKey(path.Apart))
          {
            unique.Add(path.Apart);
            map.Add(path.Apart, true);
          }
        }

        return unique;
      }

      [Benchmark()]
      public List<string> LinqUniqueAParts()
      {
        // string searchA = _rivers.PickRandom();
        return _paths.Distinct(new APartComparer()).Select(dpp => dpp.Apart).ToList();
      }

      [Benchmark()]
      public List<string> HashsetUniqueAParts()
      {
        var hs = new HashSet<string>();
        foreach (DSSPath path in _paths)
        {
          hs.Add(path.Apart);
        }
        return hs.ToList();
      }

    }

    [MemoryDiagnoser]
    public class CondensePaths
    {
      const int TotalPathCount = 100000;

      private List<DSSPath> _paths;

      public CondensePaths()
      {
        _paths = StringSortHelpers.MakePathList(TotalPathCount);
      }

      [Benchmark(Baseline = true)]
      public List<List<DSSPath>> BaselineGroupedCollection()
      {
        // old dssio implementatijon
        return _paths.OrderBy(p => p.PathWithoutDate)
                  .ThenBy(p => p.SortableDPart).GroupBy(p => p.PathWithoutDate).Select(grp => grp.ToList()).ToList();
      }

      [Benchmark()]
      public List<DSSPath> DistinctComparerJustCondensed()
      {
        return _paths.Distinct(new DSSPath.DatelessComparer()).ToList();
      }

      // Tested on battery. Barely faster, 2x GC. not worth
      //[Benchmark()]
      //public List<DSSPathProxy> DistinctComparerJustCondensedParallel()
      //{
      //  return _paths.AsParallel().Distinct(new DSSPathProxy.DatelessComparer()).ToList();
      //}

      [Benchmark()]
      public List<List<DSSPath>> DistinctComparerGroupedCollection()
      {
        return _paths.GroupBy(dssPath => dssPath, new DSSPath.DatelessComparer()).Select(grp => grp.OrderBy(dssp => dssp.DPartDT).ToList()).ToList();
      }
    }

    [MemoryDiagnoser]
    public class Interning
    {
      private const string basePath = @"M:\_Projects\HEC-DSS\test-data\benchmarks6\";

      //only doing reads, no need to copy.
      //the name of the file is the size essentially, plus the version number.
      const string kb78v6 = basePath + "Ark.dss";
      const string kb33v6 = basePath + "BaldEDmbrk.dss";
      const string gb1v6 = basePath + "gavunstdy.dss";
      const string kb336v6 = basePath + "SnakeClearwater.dss";

      private string _fileToUse;
      private bool _includeRecordType = false;
      private bool _headerInfo = false;
      private bool _locationInfo = false;
      public Interning()
      {
        //choose the file to use here.
        _fileToUse = gb1v6;
      }

      [Benchmark(Baseline = true)]
      public void GetCondensedPathNamesNotInterned()
      {
       using (DSSReader reader = new DSSReader(_fileToUse))
        {
          //var paths = reader.GetCondensedPathNamesUnInterned(_includeRecordType, _headerInfo, _locationInfo);
          //throw new NotImplementedException("No longer in the file..  It sucks trust me");
        }
      }

      [Benchmark()]
      public void GetCondensedPathNamesInterned()
      {
        using (DSSReader reader = new DSSReader(_fileToUse))
        {
          var paths = reader.GetCatalog(_includeRecordType);
        }
      }

    }


    public static class StringSortHelpers
    {
      static string[] _rivers = new[] { "Muncie", "Columbia", "Ohio", "Tennessee", "Yukon", "Amazon", "Rio Grande", "Hudson", "Colorado", "Red", "Arkansas", "Sacramento" };
      static string[] _reaches = new[] { "Upper", "Middle", "Lower", "North Fork", "South Fork", "Middle Fork", "East Fork", "West Fork" };
      static string[] _dataTypes = new[] { "PER-CUM", "FLOW", "INST-VAL", "PER-AVER", "STAGE" };
      static string[] _tsTypes = new[] { "RTS", "IRTS", "PD", "GRID" }; // e part?

      public static List<DSSPath> MakePathList(int ct)
      {
        var paths = new List<DSSPath>(ct);
        System.Random _random = new System.Random(0);

        // time is globally unique for now
        DateTime time = new DateTime(1900, 1, 1);
        for (int i = 0; i < ct; i++)
        {
          string river =_rivers[_random.Next(_rivers.Length)];
          string reach = _reaches[_random.Next(_reaches.Length)];
          string dataType = _dataTypes[_random.Next(_dataTypes.Length)];
          string tsType = _tsTypes[_random.Next(_tsTypes.Length)];
          paths.Add(new DSSPath(river, reach, dataType, time, tsType, "FPart"));
          time.AddHours(1);
        }

        return paths;
      }
    }

  }
}
