using System;
using System.Globalization;
using System.Text.RegularExpressions;
using Hec.Dss.Native;

namespace Hec.Dss
{
  public static class Time
  {
    static Time() => Assembly.Initialize();

    const string HecDateFormat = "ddMMMyyyy";
    const int UndefinedValue = -(int.MaxValue);

    static Regex hecDateRegex = new Regex(@"\d\d\w{3}\d\d\d\d", System.Text.RegularExpressions.RegexOptions.Compiled);

    static CultureInfo s_cultureInfo = new CultureInfo("en-US");

    /// <summary>
    /// Converts DateTime to HEC format ddMMMyyyy (en-US)
    /// </summary>
    /// <param name="t">DateTime </param>
    /// <returns></returns>
    private static string HecDateToString(DateTime t)
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

    internal static string JulianToHecDate(int julianBaseDate)
    {
      int y = 0, m = 0, d = 0;
      PInvoke.JulianToYearMonthDay(julianBaseDate, ref y, ref m, ref d);
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
    private static bool TryParseTimeString(string timeString, out int hr, out int min, out int sec)
    {
      
      sec = 0;
      hr = 0;
      min = 0;
      if (timeString.Length == 4)
      {
        //hr = Convert.ToInt32(timeString.Substring(0, 2));
        //min = Convert.ToInt32(timeString.Substring(2, 2));
        if (!Int32.TryParse(timeString.Substring(0, 2), out hr))
          return false;

        if (!Int32.TryParse(timeString.Substring(2, 2), out min))
          return false;
      }
      else
      if (timeString.Contains(":"))
      {
        var tokens = timeString.Split(':');
        if (tokens.Length >= 2 && tokens.Length <= 3)
        {
          //hr = Convert.ToInt32(tokens[0]);
          //min = Convert.ToInt32(tokens[1]);
          if (!Int32.TryParse(tokens[0], out hr))
            return false;

          if (!Int32.TryParse(tokens[1], out min))
            return false;

          if (tokens.Length == 3)
            //sec = Convert.ToInt32(tokens[2]);
            if (!Int32.TryParse(tokens[2], out sec))
              return false;
        }
        else
          return false; // throw new FormatException("could not parse " + timeString);
      }
      else if (timeString.Length == 4)
      {
        //hr = Convert.ToInt32(timeString.Substring(0, 2));
        //min = Convert.ToInt32(timeString.Substring(2, 2));
        if (!Int32.TryParse(timeString.Substring(0, 2), out hr))
          return false;

        if (!Int32.TryParse(timeString.Substring(2, 2), out min))
          return false;
      }
      else
      {
        return false;// throw new FormatException("could not parse " + timeString);
      }
      return true;
      
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

      date = HecDateToString(t);
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
        int secs = PInvoke.TimeStringToSeconds(times[i].ToString("HH:mm:ss"));
        rval[i] = (julian - julianBaseDate) * (86400 / timeGranularitySeconds) + (secs / timeGranularitySeconds);
      }
      return rval;
    }

    internal static int DateToJulian(DateTime t)
    {
      return PInvoke.DateToJulian(HecDateToString(t));
    }

    public static bool HecDateTimeUndefined(int julianDate)
    {
      return julianDate == UndefinedValue;
    }

  }
}
