using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Hec.Dss
{
    public class Quality
    {
      public object Flag { get; set; }
      public string Name { get; set; }
      public int Value { get; set; }
      public bool IsSet { get; set; }
      public Quality(BaseQualityFlags baseQuality, bool isSet = false)
      {
        Flag = baseQuality;
        Name = baseQuality.ToString();
        Value = (int)baseQuality;
        this.IsSet = isSet;
      }
      public Quality(PreviousModifierFlags previousModifier, bool isSet = false)
      {
        Flag = previousModifier;
        Name = previousModifier.ToString();
        Value = (int)previousModifier;
        this.IsSet = isSet;
    }
      public Quality(ReplacementMethodFlags replacementMethod, bool isSet = false)
      {
        Flag = replacementMethod;
        Name = replacementMethod.ToString();
        Value = (int)replacementMethod;
        this.IsSet = isSet;
      }
      public Quality(TestFailedFlags testFailed, bool isSet = false)
      {
        Flag = testFailed;
        Name = testFailed.ToString();
        Value = (int)testFailed;
        this.IsSet = isSet;
      }

      public Quality()
      {

      }

      public string StringQuality
      {
        get
        {
          return ToString(Value);
        }
      }

      public static List<Quality> AllFlags
      {
        get
        {
          var l = new List<Quality>();
          foreach (var item in Enum.GetValues(typeof(BaseQualityFlags)))
          {
            l.Add(new Quality((BaseQualityFlags)item));
          }
          foreach (var item in Enum.GetValues(typeof(PreviousModifierFlags)))
          {
            l.Add(new Quality((PreviousModifierFlags)item));
        }
          foreach (var item in Enum.GetValues(typeof(ReplacementMethodFlags)))
          {
            l.Add(new Quality((ReplacementMethodFlags)item));
        }
          foreach (var item in Enum.GetValues(typeof(TestFailedFlags)))
          {
            l.Add(new Quality((TestFailedFlags)item));
        }
          return l;
        }
      }

      public static string ToString(int quality)
      {
        List<string> qualityList = new List<string>();
        ActiveBaseQualities(quality, qualityList);
        ActivePreviousModifierQualities(quality, qualityList);
        ActiveReplacementMethodQualities(quality, qualityList);
        ActiveTestFailedQualities(quality, qualityList);
        return (qualityList.Count != 0) ? String.Join(", ", qualityList.ToArray()) : "";
      }

      private static void ActiveBaseQualities(int quality, List<string> ql)
      {
        var f = Enum.GetValues(typeof(BaseQualityFlags));
        var q = (BaseQualityFlags)quality;
        foreach (BaseQualityFlags flag in f)
        {
          if (q.HasFlag(flag))
            ql.Add(flag.ToString());
        }
      }

      private static void ActivePreviousModifierQualities(int quality, List<string> ql)
      {
        int pmq = (quality >> 8) & 0b111;
        if (Enum.IsDefined(typeof(PreviousModifierFlags), pmq))
          ql.Add(((PreviousModifierFlags)pmq).ToString());
      }

      private static void ActiveReplacementMethodQualities(int quality, List<string> ql)
      {
        int rmq = (quality >> 11) & 0b1111;
        if (Enum.IsDefined(typeof(ReplacementMethodFlags), rmq))
          ql.Add(((ReplacementMethodFlags)rmq).ToString());
      }

      private static void ActiveTestFailedQualities(int quality, List<string> ql)
      {
        var f = Enum.GetValues(typeof(TestFailedFlags));
        var q = (TestFailedFlags)quality;
        foreach (TestFailedFlags flag in f)
        {
          if (q.HasFlag(flag))
            ql.Add(flag.ToString());
        }
      }
      
      /// <summary>
      /// Returns a list of qualities, each quality indicating whether it's set or not in the time series point.
      /// </summary>
      /// <param name="tsp"></param>
      /// <returns></returns>
      public static List<Quality> QualityStats(TimeSeriesPoint tsp, bool GetActiveQualities = true, bool GetInActiveQualities = true)
      {
        var l = new List<Quality>();
        foreach (var item in Enum.GetValues(typeof(BaseQualityFlags)))
        {
          if (GetActiveQualities && IsQualitySet(tsp, (BaseQualityFlags)item))
            l.Add(new Quality((BaseQualityFlags)item, true));
          else if (GetInActiveQualities && !IsQualitySet(tsp, (BaseQualityFlags)item))
            l.Add(new Quality((BaseQualityFlags)item));
        }
        foreach (var item in Enum.GetValues(typeof(PreviousModifierFlags)))
        {
          if (GetActiveQualities && IsQualitySet(tsp, (PreviousModifierFlags)item))
            l.Add(new Quality((PreviousModifierFlags)item, true));
          else if (GetInActiveQualities && !IsQualitySet(tsp, (PreviousModifierFlags)item))
            l.Add(new Quality((PreviousModifierFlags)item));
        }
        foreach (var item in Enum.GetValues(typeof(ReplacementMethodFlags)))
        {
          if (GetActiveQualities && IsQualitySet(tsp, (ReplacementMethodFlags)item))
            l.Add(new Quality((ReplacementMethodFlags)item, true));
          else if (GetInActiveQualities && !IsQualitySet(tsp, (ReplacementMethodFlags)item))
            l.Add(new Quality((ReplacementMethodFlags)item));
        }
        foreach (var item in Enum.GetValues(typeof(TestFailedFlags)))
        {
          if (GetActiveQualities && IsQualitySet(tsp, (TestFailedFlags)item))
            l.Add(new Quality((TestFailedFlags)item, true));
          else if (GetInActiveQualities && !IsQualitySet(tsp, (TestFailedFlags)item))
            l.Add(new Quality((TestFailedFlags)item));
        }
        return l;
    
      }

      public static bool IsQualitySet(TimeSeriesPoint tsp, BaseQualityFlags flag)
      {
        return (tsp.IntQuality & (int)flag) == (int)flag;
      }

      public static bool IsQualitySet(TimeSeriesPoint tsp, PreviousModifierFlags flag)
      {
        int pmq = (tsp.IntQuality >> 8) & 0b111;
        return pmq  == (int)flag;
      }

      public static bool IsQualitySet(TimeSeriesPoint tsp, ReplacementMethodFlags flag)
      {
        int rmq = (tsp.IntQuality >> 11) & 0b1111;
        return rmq  == (int)flag;
      }

      public static bool IsQualitySet(TimeSeriesPoint tsp, TestFailedFlags flag)
      {
        return (tsp.IntQuality & (int)flag) == (int)flag;
      }


      [Flags]
      public enum BaseQualityFlags
      {
        Screened = 1 << 0,
        Okay = 1 << 1,
        Missing = 1 << 2,
        Questionable = 1 << 3,
        Reject = 1 << 4,
        Modified = 1 << 7,
        Protected = 1 << 31
      }

      public enum PreviousModifierFlags
      {
        OriginalValue = 0,
        DATCHK = 1,
        DATVUE = 2,
        ManuelEntry = 3,
        OriginalAcceptedDATVUE = 4
      }

      public enum ReplacementMethodFlags
      {
        NoRevision = 0,
        LinearInterpolation = 1,
        ManualChange = 2,
        ReplaceWithMissingValue = 3
      }

      [Flags]
      public enum TestFailedFlags
      {
        AbsoluteMagnitude = 1 << 15,
        ConstantValue = 1 << 16,
        RateOfChange = 1 << 17,
        RelativeMagnitude = 1 << 18,
        DurationMagnitude = 1 << 19,
        NegativeIncrementalValue = 1 << 20,
        NotDefined = 1 << 21,
        GageList = 1 << 22,
        UserDefinedTest = 1 << 23,
        DistributionTest = 1 << 24
      }

    }
}
