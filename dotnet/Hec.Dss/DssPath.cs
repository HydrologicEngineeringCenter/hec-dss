using System;
using System.Linq;
using System.Diagnostics;
using System.Collections.Generic;

namespace Hec.Dss
{
  [DebuggerDisplay("{FullPath}")]
  public class DssPath : IComparable<DssPath>
  {
    private string _Apart;
    private string _Bpart;
    private string _Cpart;
    private string _Dpart;
    private string _Epart;
    private string _Fpart;

    public RecordType RecordType { get; internal set; }
    public double XOrdinate { get; internal set; }
    public double YOrdinate { get; internal set; }
    public double ZOrdinate { get; internal set; }

    public DateTime DPartAsDateTime 
    { 
      get
      {
        if (Time.TryConvertFromHecDateTime(Dpart, out DateTime date))
          return date;

        return default(DateTime);
      }
    }

    public DateTime EPartAsDateTime
    {
      get
      {
        if (Time.TryConvertFromHecDateTime(Epart, out DateTime date))
          return date;

        return default(DateTime);
      }
    }

    /// <summary>
    /// Valid for all types of data except for paired data
    /// </summary>
    /// <param name="date"></param>
    /// <returns></returns>
    public bool TryParseDPartAsDateTime(out DateTime date)
    {
      return Time.TryConvertFromHecDateTime(_Dpart, out date);
    }

    /// <summary>
    /// Only used for some grids
    /// </summary>
    /// <param name="date"></param>
    /// <returns></returns>
    public bool TryParseEPartAsDateTime(out DateTime date)
    {
      return Time.TryConvertFromHecDateTime(_Epart, out date);
    }

    // header information
    public string Units { get; internal set; }
    public string DataType { get; internal set; }

    public bool HasValidCoordinates
    {
      get
      {
        return
             this.XOrdinate != Constant.UNDEFINED_LOCATION_INFO_VALUE
          && this.YOrdinate != Constant.UNDEFINED_LOCATION_INFO_VALUE
          && this.ZOrdinate != Constant.UNDEFINED_LOCATION_INFO_VALUE;

      }
    }

    /// <summary>
    /// Returns true if this path represents a regular interval time series
    /// </summary>
    /// <returns></returns>
    public bool IsRegular
    {
      get
      {

        if (Epart.StartsWith("IR-", StringComparison.OrdinalIgnoreCase))
          return false;
        if (Epart.StartsWith("~", StringComparison.OrdinalIgnoreCase))
          return false;


        return true;
      }
    }

    public bool DPartIsDate()
    {
      return DPartAsDateTime != default(DateTime);
    }

    public bool DPartIsDateOrDateRange()
    {
      if (DPartIsDate())
        return true;

      //not a date, but could be a date range
      var splitByDash = Dpart.Split('-');
      if (splitByDash.Length == 2)
      {
        return Time.TryConvertFromHecDateTime(splitByDash[0], out _) && Time.TryConvertFromHecDateTime(splitByDash[1], out _);
      }
      else
        return false;
    }

    /// <summary>
    /// returns a path without the Date (part D) 
    /// used to sort by all parts, except Date
    /// </summary>
    public string PathWithoutDate
    {
      get
      {
        return "/" + Apart + "/" + Bpart + "/" + Cpart + "//"
             + Epart + "/" + Fpart + "/";

      }
    }

    /// <summary>
    /// Returns D part with the first Date in the range
    /// </summary>
    internal string PathWithoutRange
    {
      get
      {
        var D = Dpart;
        int idx = D.IndexOf("-");
        if (idx > 0)
        {
          D = D.Substring(0, idx);
        }
        return "/" + Apart + "/" + Bpart + "/" + Cpart + "/" + D + "/"
           + Epart + "/" + Fpart + "/";
      }
    }

    /// <summary>
    /// returns date portion (D) of path in a sortable format ToString("yyyyMMdd");
    /// </summary>
    public virtual string SortableDPart
    {
      get
      {
        if (!Time.TryConvertFromHecDateTime(Dpart, out DateTime date))
          return Dpart;
        else
          return date.ToString("yyyyMMdd");
      }
    }

    public string Apart
    {
      get
      {
        return _Apart;
      }
      set
      {
        _Apart = value;
      }
    }

    public string Bpart
    {
      get
      {
        return _Bpart;
      }
      set
      {
        _Bpart = value;
      }
    }

    public string Cpart
    {
      get
      {
        return _Cpart;
      }
      set
      {
        _Cpart = value;
      }
    }

    public string Dpart
    {
      get
      {
        return _Dpart;
      }
      set
      {
        _Dpart = value;
      }
    }

    public string Epart
    {
      get
      {
        return _Epart;
      }
      set
      {
        _Epart = value;
      }
    }

    public string Fpart
    {
      get
      {
        return _Fpart;
      }
      set
      {
        _Fpart = value;
      }
    }

    public string FullPath
    {
      get
      {
        return "/" + _Apart.Trim() + "/" + _Bpart.Trim() + "/" + _Cpart.Trim() + "/"
          + _Dpart.Trim() + "/" + _Epart.Trim() + "/" + _Fpart.Trim() + "/";
      }
    }

    public override string ToString()
    {
      return FullPath;
    }

    public static bool EqualsDateLess(DssPath x, DssPath y)
    {
      var cmp = new DatelessComparer();
      return cmp.Equals(x, y);
    }

    public static DssPath NotFound = new DssPath();

    public DssPath()
    {
      _Apart = "";
      _Bpart = "";
      _Cpart = "";
      _Dpart = "";
      _Epart = "";
      _Fpart = "";
    }

    public DssPath(string A = "", string B = "", string C = "", string D = "", string E = "", string F = "", RecordType recordType = RecordType.Unknown,
      string dataType = "", string dataUnits = "", double xOrdinate = Constant.UNDEFINED_LOCATION_INFO_VALUE, double yOrdinate = Constant.UNDEFINED_LOCATION_INFO_VALUE, double zOrdinate = Constant.UNDEFINED_LOCATION_INFO_VALUE)
    {
      _Apart = A;
      _Bpart = B;
      _Cpart = C;
      _Dpart = D;
      _Epart = E;
      _Fpart = F;

      RecordType = recordType;
      DataType = dataType;
      Units = dataUnits;
      XOrdinate = xOrdinate;
      YOrdinate = yOrdinate;
      ZOrdinate = zOrdinate;
    }

    public DssPath(string A = "", string B = "", string C = "", DateTime D = default(DateTime), string E = "", string F = "", RecordType recordType = RecordType.Unknown,
      string dataType = "", string dataUnits = "", double xOrdinate = Constant.UNDEFINED_LOCATION_INFO_VALUE, double yOrdinate = Constant.UNDEFINED_LOCATION_INFO_VALUE, double zOrdinate = Constant.UNDEFINED_LOCATION_INFO_VALUE)
    {
      _Apart = A;
      _Bpart = B;
      _Cpart = C;
      _Dpart = D.ToString("yyyyMMdd");
      _Epart = E;
      _Fpart = F;

      RecordType = recordType;
      DataType = dataType;
      Units = dataUnits;
      XOrdinate = xOrdinate;
      YOrdinate = yOrdinate;
      ZOrdinate = zOrdinate;
    }

    public bool IsTimeSeries()
    {
      if (RecordType == RecordType.RegularTimeSeries || RecordType == RecordType.IrregularTimeSeries
         || RecordType == RecordType.RegularTimeSeriesProfile)
        return true;

      if (RecordType == RecordType.Unknown) // estimate based on path format.
      {
        //A time series will have a Date in the DPart (or be blank)
        // and EPart will have valid interval or block size
        if ((Dpart.Trim() == "" || DPartIsDateOrDateRange())
          && TimeWindow.SecondsInInterval(this) != 0)
          return true;
      }

      return false;
    }

    public DssPath(string path)
    {
      if (path == null)
        return;

      if (!path.StartsWith("/"))
        path = "/" + path;

      if (!path.EndsWith("/"))
        path = path + "/";

      string[] splitPath = path.Split('/');
      if (splitPath.Length != 8)
      {
        throw new Exception("Invalid path given");
      }
      _Apart = splitPath[1];
      _Bpart = splitPath[2];
      _Cpart = splitPath[3];
      _Dpart = splitPath[4];
      _Epart = splitPath[5];
      _Fpart = splitPath[6];

      RecordType = RecordType.Unknown;
      DataType = "";
      Units = "";
    }

    public static bool IsValid(string path)
    {
      string[] splitPath = path.Split('/');
      if (splitPath.Length != 8)
        return false;

      return true;
    }

    public static bool TryParse(string pathname, ref DssPath dssPath)
    {
      // Some sources may not prepend or postpend the appropriate slashes.... try to fix it
      if (!pathname.StartsWith("/"))
        pathname = "/" + pathname;

      if (!pathname.EndsWith("/"))
        pathname = pathname + "/";

      string[] splitPath = pathname.Split('/');
      if (splitPath.Length != 8)
        return false;

      dssPath = new DssPath(pathname);
      return true;
    }

    /// <summary>
    /// Tells you if DPart is a range or not
    /// </summary>
    /// <returns>true if DPart is a range (contains a -), or false if it does not contain a -.</returns>
    public bool IsDPartARange()
    {
      return (_Dpart.Split('-').Length == 2);
    }

    /// <summary>
    /// Tells you if Epart is a date string or not
    /// </summary>
    /// <returns></returns>
    public bool IsEPartADate()
    {
      DateTime temp;
      return DateTime.TryParse(_Epart, out temp);
    }

    /// <summary>
    /// Gets the Dpart as a DateTime Object
    /// </summary>
    /// <returns></returns>
    public DateTime GetDPartAsDateTime()
    {
      if (IsDPartARange())
      {// return the starting date
        var t1 = _Dpart.Split('-')[0];
        return Time.ConvertFromHecDateTime(t1);
      }
      return Time.ConvertFromHecDateTime(_Dpart);
    }

    /// <summary>
    /// Gets the Epart as a DateTime Object
    /// </summary>
    /// <returns></returns>
    public DateTime GetEPartAsDateTime()
    {
      return Time.ConvertFromHecDateTime(_Epart);
    }

    public int CompareTo(DssPath y)
    {
      int res = Apart.CompareTo(y.Apart);
      if (res != 0)
        return res;

      res = Bpart.CompareTo(y.Bpart);
      if (res != 0)
        return res;

      res = Cpart.CompareTo(y.Cpart);
      if (res != 0)
        return res;

      // Dates
      DateTime xDate = DPartAsDateTime;
      DateTime yDate = y.DPartAsDateTime;
      if (xDate == default || yDate == default)
      {
        // Someone couldn't be parsed as datetime
        res = Dpart.CompareTo(y.Dpart);
        if (res != 0)
          return res;
      }
      else
      {
        // Just compare datetimes
        res = xDate.CompareTo(yDate);
        if (res != 0)
          return res;
      }

      res = Epart.CompareTo(y.Epart);
      if (res != 0)
        return res;

      // Example collection path
      // /RUSSIANNAPA/APCC1/FLOW/01SEP2019/1HOUR/C:000002|T:0212019 
      const int ExpectedCollectionLen = 18;

      // F part is special with collections
      if (!Fpart.StartsWith("C:") || Fpart.Length != ExpectedCollectionLen)
      {
        // No collection, handle normally
        return Fpart.CompareTo(y.Fpart);
      }
      else
      {
        // We may want to make this more robust later, but for now,
        // starting with C: and being 18 chars is a very good indicator that it's a collection!

        //var tokens = x.Fpart.Split('|');

        //// Isn't a known collection entity, just string compare
        //if (tokens.Length != 2)
        //  return x.Fpart.CompareTo(y.Fpart);

        // Sort on the T:XXX token first, then the C:XXX token          
        // Pretend it will always have the same number of chars
        res = string.Compare(Fpart, 11, y.Fpart, 11, 7);
        if (res != 0)
          return res;

        // Compare the c part last
        return string.Compare(Fpart, 2, y.Fpart, 2, 6);
      }
    }

    static string[] minutesV7 ={ "30Minute",  "20Minute",   "15Minute",
    "12Minute",   "10Minute",  "6Minute",    "5Minute",
    "4Minute",    "3Minute",   "2Minute",    "1Minute"};

    static string[] minutesV6 ={  "30MIN",         "20MIN",     "15MIN",
    "12MIN",   "10MIN",          "6MIN",      "5MIN",
    "4MIN",       "3MIN",          "2MIN",      "1MIN" };
    private const int partEMinLength = 4;
    private const int partEMaxLength = 8;

    /// <summary>
    /// compares part E, possibly different formats.
    /// Focus on xxMinute and xxMIN since the Minute has differences in in V6 anv V7.
    /// </summary>
    /// <param name="x"></param>
    /// <param name="y"></param>
    /// <returns></returns>
    private static bool CompareEParts(string x, string y)
    {
      if (String.Compare(x, y, true) == 0) // may be the same format.
        return true;

      if (string.IsNullOrEmpty(x) || string.IsNullOrEmpty(y))
        return false;
      if (x.Length < partEMinLength || y.Length < partEMinLength)
        return false;
      if (x.Length > partEMaxLength || y.Length > partEMaxLength)
        return false;


      // do we need to pre sort the minutesV7 and minutesV6?
      int x6 = Array.FindIndex(minutesV6, t => t.Equals(x, StringComparison.InvariantCultureIgnoreCase));
      if (x6 >= 0)
      {
        int y7 = Array.FindIndex(minutesV7, t => t.Equals(y, StringComparison.InvariantCultureIgnoreCase));
        return y7 == x6;
      }

      int x7 = Array.FindIndex(minutesV7, t => t.Equals(x, StringComparison.InvariantCultureIgnoreCase));
      if (x7 >= 0)
      {
        int y6 = Array.FindIndex(minutesV6, t => t.Equals(y, StringComparison.InvariantCultureIgnoreCase));
        return y6 == x7;
      }

      return false;
    }


    public class DatelessComparer : IEqualityComparer<DssPath>
    {
      public bool Equals(DssPath x, DssPath y)
      {
        return String.Compare(x.Apart, y.Apart, true) == 0 &&
               String.Compare(x.Bpart, y.Bpart, true) == 0 &&
               String.Compare(x.Cpart, y.Cpart, true) == 0 &&
               DssPath.CompareEParts(x.Epart, y.Epart) &&
               String.Compare(x.Fpart, y.Fpart, true) == 0;
      }
      public int GetHashCode(DssPath obj)
      {
        return CombineHashCodes(obj.Apart, obj.Bpart, obj.Cpart, obj.Epart, obj.Fpart);
      }

      //Note: Consider moving to a utility library if this gets reused
      public static int CombineHashCodes<T>(params T[] inputs)
      {
        int hash = 5381;

        foreach (var obj in inputs)
        {
          int hashCode = obj.GetHashCode();
          hash = ((hash << 5) + hash) ^ hashCode;
        }

        return hash;
      }
    }

    class PathComparer : IComparer<DssPath>
    {
      public int Compare(DssPath x, DssPath y)
      {
        int res = x.Apart.CompareTo(y.Apart);
        if (res != 0)
          return res;

        res = x.Bpart.CompareTo(y.Bpart);
        if (res != 0)
          return res;

        res = x.Cpart.CompareTo(y.Cpart);
        if (res != 0)
          return res;

        // Dates
        DateTime xDate = x.DPartAsDateTime;
        DateTime yDate = y.DPartAsDateTime;
        if (xDate == default || yDate == default)
        {
          // Someone couldn't be parsed as datetime
          res = x.Dpart.CompareTo(y.Dpart);
          if (res != 0)
            return res;
        }
        else
        {
          // Just compare datetimes
          res = xDate.CompareTo(yDate);
          if (res != 0)
            return res;
        }

        res = x.Epart.CompareTo(y.Epart);
        if (res != 0)
          return res;

        // F part is special with collections
        if (!x.Fpart.StartsWith("C:") || x.Fpart.Length != ExpectedCollectionLen)
        {
          // No collection, handle normally
          return x.Fpart.CompareTo(y.Fpart);
        }
        else
        {
          // We may want to make this more robust later, but for now,
          // starting with C: and being 18 chars is a very good indicator that it's a collection!

          //var tokens = x.Fpart.Split('|');

          //// Isn't a known collection entity, just string compare
          //if (tokens.Length != 2)
          //  return x.Fpart.CompareTo(y.Fpart);

          // Sort on the T:XXX token first, then the C:XXX token          
          // Pretend it will always have the same number of chars
          res = string.Compare(x.Fpart, 11, y.Fpart, 11, 7);
          if (res != 0)
            return res;

          // Compare the c part last
          return string.Compare(x.Fpart, 2, y.Fpart, 2, 6);
        }

        // return string.Compare(CollectionSortable(x), CollectionSortable(y));
      }
      const int ExpectedCollectionLen = 18;

      //            string previousT = dssPaths[0].Fpart.Split('|').Last().Split(':').Last();
      //          string previousLoc = dssPaths[0].Bpart;
      // /RUSSIANNAPA/APCC1/FLOW/01SEP2019/1HOUR/C:000002|T:0212019 

      private string CollectionSortable(DssPath x)
      {
        string rval = x.Apart + x.Bpart + x.Cpart + x.SortableDPart + x.Epart;

        if (!x.Fpart.StartsWith("C:"))
          return rval;

        var tokens = x.Fpart.Split('|');
        if (tokens.Length != 2)
          return x.PathWithoutDate;
        rval += tokens[1].Split(':')[1] + tokens[0].Split(':')[1];
        return rval;
      }
    }

  }
}
