using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Hec.Dss
{
  public static class TimeWindow
  {

    /// <summary>
    /// Get a set of data between two dates in a TimeSeries
    /// trims data outside t1,t2
    /// 
    /// </summary>
    /// <param name="ts">TimeSeries</param>
    /// <param name="t1">Start Date</param>
    /// <param name="t2">End Date</param>
    /// <returns></returns>
    public static TimeSeries Trim(TimeSeries ts, DateTime t1, DateTime t2)
    {
      if (ValidWindowForTrim(ts, t1, t2))
      {
        TrimLeft(ts, t1, t2);
        TrimRight(ts, t1, t2);
      }
      return ts;
    }
    private static bool ValidWindowForTrim(TimeSeries ts, DateTime t1, DateTime t2)
    {
      if( ts.Count == 0 ) return false;
      if( t2 < t1) return false;
      var last = ts.Times[ts.Count - 1];

      // time ranges before data?
      if (t1 > last) return false;
      if (t2 < ts.StartDateTime) return false;

      return true;
    }

    /// <summary>
    /// Remove dates and values in the range specfied
    /// </summary>
    /// <param name="ts"></param>
    /// <param name="Start"></param>
    /// <param name="End"></param>
    private static void RemoveRange(TimeSeries ts, int Start, int End)
    {
      var l = new List<DateTime>(ts.Times);
      var d = new List<double>(ts.Values);
      l.RemoveRange(Start, End);
      d.RemoveRange(Start, End);
      ts.Times = l.ToArray();
      ts.Values = d.ToArray();
    }

    private static void TrimLeft(TimeSeries ts, DateTime t1, DateTime t2)
    {
      if (ts.Count == 0 || t1 < ts.StartDateTime)
        return;

      int idx = Array.FindIndex(ts.Times, t => t >= t1);
      if (ts.Times[idx] != t1) // need to backup one time-step
      {
        idx--;
      }
      RemoveRange(ts, 0, idx);
      return;

    }    

    private static void TrimRight(TimeSeries ts, DateTime t1, DateTime t2)
    {
      if (ts.Count == 0 || t2 > ts.Times[ts.Count-1])
        return;

      int idx = Array.FindLastIndex(ts.Times, t => t <= t2);

      if (ts.Times[idx] == t2)
      {
        idx += 1;
      }else
      {
        idx += 2;
      }

      RemoveRange(ts, idx, ts.Times.Length - idx);
    }

    internal static int SecondsInInterval(TimeSeries ts)
    {
      return SecondsInInterval(ts.Path);
    }

    internal static int SecondsInInterval(DssPath dssPath)
    {
      int seconds = 0;
      for (int i = 0; i < TimeIntervals7.Length; i++) // look through v7 time intervals
      {
        if (String.Compare(dssPath.Epart.TrimStart('~'), TimeIntervals7[i], true) == 0)
        {
          seconds = TimeIntervalSeconds7[i];
          break;
        }
      }
      if (seconds == 0) // look through v6 time intervals
      {
        for (int i = 0; i < TimeIntervals6.Length; i++)
        {
          if (String.Compare(dssPath.Epart.TrimStart('~'), TimeIntervals6[i], true) == 0)
          {
            seconds = TimeIntervalSeconds6[i];
            break;
          }
        }
      }
      return seconds;
    }

    public static string GetInterval(TimeSeries ts)
    {
      var t = ts.TimeSpanInterval();
      var i = Array.IndexOf(TimeIntervalSeconds7, (int)t.TotalSeconds);
      try
      {
        return TimeIntervals7[i];
      }
      catch (IndexOutOfRangeException)
      {
        return "IR-YEAR";
      }

    }

    public static string GetInterval(TimeSpan ts)
    {
      var i = Array.IndexOf(TimeIntervalSeconds7, (int)ts.TotalSeconds);
      try
      {
        return TimeIntervals7[i];
      }
      catch (IndexOutOfRangeException)
      {
        return "IR-YEAR";
      }
    }

    /// <summary>
    /// Behavior for time windows when getting a Time Series record
    /// </summary>
    public enum TimeWindowBehavior 
    { 
      /// <summary>
      /// return data that is strictly within the time window.
      /// </summary>
      StrictlyInclusive, 
      /// <summary>
      /// Extend data if needed to cover/support interpolation within the time window.
      /// </summary>
      Cover 
    };

    /// <summary>
    /// Flags for retrieving regular Time Series
    /// </summary>
    public enum RegularRetrieve 
    {
      /// <summary>
      /// Adhere to time window provided and generate the time array.
      /// </summary>
      AdhereWithTimeArray,
      /// <summary>
      /// Trim data.  Remove missing values at the beginning and end of data set (not inside), and generate the time array.
      /// </summary>
      TrimWithTimeArray,
      /// <summary>
      /// Adhere to time window provided but do not include time array.
      /// </summary>
      AdhereWithoutTimeArray,
      /// <summary>
      /// Trim data.  Remove missing values at the beginning and end of data set (not inside), no time array.
      /// </summary>
      TrimWithoutTimeArray
    };

    /// <summary>
    /// Flags for retrieving irregular Time Series
    /// </summary>
    public enum IrregularRetrieve 
    {
      /// <summary>
      /// Adhere to time window provided.
      /// </summary>
      Adhere,
      /// <summary>
      /// Retrieve one value previous to start of time window.
      /// </summary>
      RetrieveBefore,
      /// <summary>
      /// Retrieve one value after end of time window.
      /// </summary>
      RetrieveAfter,
      /// <summary>
      /// Retrieve one value before and one value after time window.
      /// </summary>
      RetrieveBeforeAndAfter
    };

    public enum ConsecutiveValueCompression
    { 
      /// <summary>
      /// Default behavior
      /// </summary>
      None,
      /// <summary>
      /// Trim out (or never read) spans of no data (keep first no data and last)
      /// </summary>
      NoData,
      /// <summary>
      /// Trim out (or never read) spans of no data and data with consecutive zeros (keep first no data and/or zero and last)
      /// </summary>
      ZeroAndNoData,
      /// <summary>
      /// Trim out (or never read) spans of consecutive data (keep first consecutive data last)
      /// </summary>
      AnyValue
    }

    private static string[] TimeIntervals7 =
    {
      "1Year",      "1Month",    "Semi-Month", "Tri-Month",
      "1Week",      "1Day",      "12Hour",     "8Hour",
      "6Hour",      "4Hour",     "3Hour",      "2Hour",
      "1Hour",      "30Minute",  "20Minute",   "15Minute",
      "12Minute",   "10Minute",  "6Minute",    "5Minute",
      "4Minute",    "3Minute",   "2Minute",    "1Minute",
      "30Second",   "20Second",  "15Second",   "10Second",
      "6Second",    "5Second",   "4Second",    "3Second",
      "2Second",    "1Second",
      "IR-Century", "IR-Decade", "IR-Year", "IR-Month",  "IR-Day"
    };

    private static string[] TimeIntervals6 =
    {
      "1YEAR",      "1MON",    "SEMI-MONTH", "TRI-MONTH",
      "1WEEK",      "1DAY",        "12HOUR",     "8HOUR",
      "6HOUR",     "4HOUR",         "3HOUR",     "2HOUR",
      "1HOUR",     "30MIN",         "20MIN",     "15MIN",
      "12MIN",   "10MIN",          "6MIN",      "5MIN",
      "4MIN",       "3MIN",          "2MIN",      "1MIN",
      "IR-CENTURY", "IR-DECADE",  "IR-YEAR",  "IR-MONTH",  "IR-DAY"
    };

    private static int[] TimeIntervalSeconds7 =
    {
      31536000,     2592000,     1296000,      864000,
      604800,       86400,       43200,        28800,
      21600,        14400,       10800,        7200,
      3600,         1800,        1200,         900,
      720,          600,         360,          300,
      240,          180,         120,          60,
      30,           20,          15,           10,
      6,            5,           4,            3,
      2,            1,
      -5,          -4,          -3,           -2,      -1
    };

    private static int[] TimeIntervalSeconds6 =
    {
      31536000,     2592000,     1296000,      864000,
      604800,       86400,       43200,        28800,
      21600,        14400,       10800,        7200,
      3600,         1800,        1200,         900,
      720,          600,         360,          300,
      240,          180,         120,          60,
      -5,          -4,          -3,           -2,      -1
    };

    
  }
}
