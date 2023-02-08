using System;
using System.Globalization;
using System.Text.RegularExpressions;

namespace Hec.Dss
{
  public static class Time
  {
    const string HecDateFormat = "ddMMMyyyy";

    static Regex hecDateRegex = new Regex(@"\d\d\w{3}\d\d\d\d", System.Text.RegularExpressions.RegexOptions.Compiled);

    static CultureInfo s_cultureInfo = new CultureInfo("en-US");

    const int UNDEFINED_TIME = -2147483647;

    internal static bool IsUndefinedTime(int t)
    {
      return t == UNDEFINED_TIME;
    }

    /// <summary>
    /// Converts DateTime to HEC format ddMMMyyyy (en-US)
    /// </summary>
    /// <param name="t">DateTime </param>
    /// <returns></returns>
    private static string ToHecDateString(DateTime t)
    {
      return t.ToString(HecDateFormat, s_cultureInfo);
    }

    /// <summary>
    /// parse date time like  '04Jan1877',  '2400' 
    /// 
    /// or                   '01Nov2019', '01:01:00'
    /// if hour is 24, increment to next day
    /// example from Java: HecTime t= new HecTime("19Jan0201", "2400");
    /// </summary>
    /// <param name="dateString"></param>
    /// <param name="timeString"></param>
    /// <returns></returns>
    public static DateTime ConvertFromHecDateTime(string dateString, string timeString)
    {
      DateTime t = DateTime.ParseExact(dateString, HecDateFormat, CultureInfo.InvariantCulture);
      TryParseTimeString(timeString, out int hr, out int min, out int sec);
      t = IncrementDateTime(t, hr, min, sec);
      return t;
    }

    private static DateTime IncrementDateTime(DateTime t, int hr, int min, int sec)
    {
      if (hr == 24)
      {
        t = t.AddDays(1);
      }
      else
      {
        t = t.AddHours(hr).AddMinutes(min).AddSeconds(sec);
      }

      return t;
    }

    public static bool TryConvertFromHecDateTime(string dateString, string timeString, out DateTime date)
    {
      bool rval = DateTime.TryParseExact(dateString, HecDateFormat,
         CultureInfo.InvariantCulture, DateTimeStyles.None, out date);

      if (!rval)
        return false;

      if (!TryParseTimeString(timeString, out int hr, out int min, out int sec))
        return false;
      date = IncrementDateTime(date, hr, min, sec);
      return true;
    }
    /// <summary>
    /// Convert HecDateTime as a single string into DateTime 
    /// 31JAN2016:2400
    /// </summary>
    /// <param name=""></param>
    /// <returns></returns>
    public static DateTime ConvertFromHecDateTime(string dateTimeString)
    {
      if (!TryConvertFromHecDateTime(dateTimeString, out DateTime t))
        throw new FormatException("invalid Hec Date format '" + dateTimeString + "'");
      return t;
    }

    internal static string JulianToHecDate(int julian)
    {
      int y = 0, m = 0, d = 0;
      DssNative.hec_dss_julianToYearMonthDay(julian, ref y, ref m, ref d);
      DateTime t = new DateTime(y, m, d);

      return t.ToString(HecDateFormat);

    }

    
    /// <summary>
    /// Converts from HEC style to a .net DateTime
    /// Example inputs:
    /// 
    /// 31JAN2016:2400
    /// 01JAN1900 17:05:02
    /// 01JAN1900 17:05
    /// 
    /// </summary>
    /// <param name="dateTimeString"></param>
    /// <param name="date"></param>
    /// <returns></returns>
    public static bool TryConvertFromHecDateTime(string dateTimeString, out DateTime date)
    {
      date = default(DateTime);
      bool rval = false;
      dateTimeString = dateTimeString.Trim();

      if (!hecDateRegex.IsMatch(dateTimeString))
        return false;

      string[] tokens = dateTimeString.Trim().Split(':');

      if (tokens.Length == 2 && tokens[0].Length == 9 && tokens[1].Length == 4)
      { //31JAN2016:2400
        return TryConvertFromHecDateTime(tokens[0], tokens[1], out date);
      }

      if (dateTimeString.Length == 9)
      {//01JAN2016
        rval = DateTime.TryParseExact(dateTimeString, HecDateFormat,
            CultureInfo.InvariantCulture, DateTimeStyles.None, out date);
      }
      if (!rval)
      {// 01JAN1900 17:05:02
        tokens = dateTimeString.Split();
        if (tokens.Length == 2)
        {
          DateTime t = DateTime.ParseExact(tokens[0], HecDateFormat, CultureInfo.InvariantCulture);
          if (!TryParseTimeString(tokens[1], out int hr, out int min, out int sec))
          {
            date = default(DateTime);
            return false;
          }
          t = IncrementDateTime(t, hr, min, sec);
          date = t;
          rval = true;
        }
      }


      return rval;
    }


    /// <summary>
    ///  parse a time String into hr,min,s integers
    /// 
    /// inputs:
    /// 2400
    /// 12:00
    /// 12:02:03
    /// 
    /// </summary>
    /// <param name="timeString"></param>
    /// <param name="hr"></param>
    /// <param name="min"></param>
    /// <param name="sec"></param>
    /// <returns></returns>
    private static bool TryParseTimeString(string time, out int hr, out int min, out int sec)
    {
      sec = 0;
      hr = 0;
      min = 0;

      var timeString = time.Trim();
      if (timeString.Length < 4)
        return false;

      if (timeString.Length == 4) // 2300
      {
        if (!Int32.TryParse(timeString.Substring(0, 2), out hr))
          return false;

        if (!Int32.TryParse(timeString.Substring(2, 2), out min))
          return false;

        return true;
      }

      var tokens = timeString.Split(':');
      if (tokens.Length >= 2 && tokens.Length <= 3)
      {
        if (!Int32.TryParse(tokens[0], out hr))
          return false;

        if (!Int32.TryParse(tokens[1], out min))
          return false;

        if (tokens.Length == 3)
          if (!Int32.TryParse(tokens[2], out sec))
            return false;

        return true;
      }

      return false;
      
    }

    /// <summary>
    /// Convert from .net DateTime to DSS Date Time strings
    ///  .net                   DSS
    /// -------------        ------------------
    /// 02Jan1861 0000  -->  01Jan1861 2400 
    /// DSS Date Time does not allow 0000  
    /// .net does not allow 24:00
    /// </summary>
    /// <param name="t1"></param>
    /// <param name="date"></param>
    /// <param name="time"></param>
    public static void DateTimeToHecDateTime(DateTime t, out string date, out string time)
    {
      if( t == default(DateTime))
      {
        date = "";
        time = "";
        return;
      }
      
      if (t == t.Date) // 00:00
      {
        t = t.AddDays(-1);
        time = "2400";
      }
      else
      {
        if (t.Second > 0)
          time = t.ToString("HH:mm:ss");
        else
          time = t.ToString("HHmm");
      }

      date = ToHecDateString(t);
    }

    
    /// <summary>
    /// 	julian = dateToJulian("20June2015");
    /// 	secs = timeStringToSeconds("11:30");
    /// 	times(i) = ((julian – julianBaseDate) * 86400 / timeGranularitySeconds) + 
    /// 	(secs / timeGranularitySeconds); 
    /// </summary>
    /// <param name="times"></param>
    /// <returns></returns>
    internal static int[] ConvertDatesToHecInteger(DateTime[] times, int julianBaseDate = 0, int timeGranularitySeconds = 60)
    {
      int[] rval = new int[times.Length];
      for (int i = 0; i < times.Length; i++)
      {
        int julian = DateToJulian(times[i]);
        int secs = TimeStringToSeconds(times[i].ToString("HH:mm:ss"));
        rval[i] = (julian - julianBaseDate) * (86400 / timeGranularitySeconds) + (secs / timeGranularitySeconds);
      }
      return rval;
    }
    /// <summary>
    /// Example				Returns
    ///   0830               30600
    ///   08:30              30600
    ///   08:30:43           30643
    /// </summary>
    /// <param name="time">time string</param>
    /// <returns></returns>
    public static int TimeStringToSeconds(string time)
    {
      if (!TryParseTimeString(time, out int hr, out int min, out int sec))
        return 0;
      int rval = (hr * 3600) + (min * 60) + sec;
      return rval;
    }

    /// <summary>
    /// Julian day, in days since 01Jan1900, with 01Jan1900 being day 1 (31Dec1899 is day 0)
    /// </summary>
    /// <param name="year"></param>
    /// <param name="month"></param>
    /// <param name="day"></param>
    /// <returns></returns>
    /// <exception cref="NotImplementedException"></exception>
    public static int YearMonthDayToJulian(int year, int month, int day)
    {
      DateTime date = new DateTime(year, month, day);
      var oa = date.ToOADate() - 1;
      int rval = (int)oa;
      return rval; 
    }

    internal static DateTime[] DateTimesFromJulianArray(int[] timesJulian, int timeGranularitySeconds, int julianBaseDate)
    {
      if (timesJulian == null)
        throw new NullReferenceException("Time Series Times array was null.  Something didn't work right in DSS.");

      DateTime[] times = new DateTime[timesJulian.Length];

      double divisor = (60d * 60d * 24d) / timeGranularitySeconds;

      for (int j = 0; j < times.Length; j++)
      {
        // There appears to be an off-by-1-day error common to julian dates - DEC 1899 vs JAN 1900
        times[j] = DateTime.FromOADate((timesJulian[j] / divisor) + julianBaseDate + 1);
      }
      return times;
    }

    internal static int DateToJulian(DateTime t)
    {
      throw new NotImplementedException();
      //return DSS.DateToJulian(HecDateToString(t));
    }

    internal static void JulianToHecDateTime(int julian, int seconds, out string date, out string time)
    {
      int year = 0, month = 0, day = 0;
      DssNative.hec_dss_julianToYearMonthDay(julian, ref year, ref month, ref day);
      DateTime t = new DateTime(year, month, day);
      t = t.AddSeconds(seconds);
      DateTimeToHecDateTime(t, out date, out time);
    }
  }
}
