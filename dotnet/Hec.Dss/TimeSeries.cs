using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using static Hec.Dss.TimeWindow;

namespace Hec.Dss
{
  public class TimeSeries
  {
    private bool _compressed = false;
    public bool Compressed { get { return _compressed; } }
    static TimeSeries() => Assembly.Initialize();

    public override string ToString()
    {
      return Path + " " + Values.Length + " items "; 
    }

    public TimeSeries(string fullPath, double[] values, DateTime dateTime, string units, string dataType)
    {
      Init(new DssPath(fullPath), values, dateTime, units, dataType);
    }

    public TimeSeries(DssPath path, double[] values, DateTime dateTime, string units, string dataType)
    {
      Init(path, values, dateTime, units, dataType);
    }

    private void Init(DssPath path, double[] values, DateTime dateTime, string units, string dataType)
    {
      this.Path = path;
      this.Values = values;
      this.Times = new DateTime[0];
      this.StartDateTime = dateTime;
      this.Units = units;
      this.DataType = dataType;
    }

    public TimeSeries()
    {
      Units = "";
      DataType = "";
      Values = new double[0];
      Times = new DateTime[0];
    }

    public int IndexOf(DateTime t)
    {
      for (int i = 0; i < Times.Length; i++)
      {
        if (Times[i].CompareTo(t) == 0)
          return i;
      }
      return -1;
    }
    public void WriteToConsole()
    {
      for (int i = 0; i < Times.Length; i++)
      {
        Console.WriteLine(Times[i].ToString() + " " + Values[i]);
      }

    }

    /// <summary>
    /// StartDateTime is the DateTime of the first value in the data
    /// This is used for Regular Interval timeseries.
    /// </summary>
    public DateTime StartDateTime
    {
      get
      {
        if ( Times.Length == 0)
          throw new NullReferenceException("Error StartDateTime is not defined");
        else return Times[0];
      }
      set
      {
        if (Times.Length == 0)
          Times = new DateTime[1] { value };
        else if (Times.Length > 0)
        {
          Times[0] = value;
        }
      }
    }

    public double[] Values { get; set; }
    public int[] Qualities { get; set; }
    public DateTime[] Times { get; set; }

    /// <summary>
    /// %,ft2,ppm,umho/cm, unit,$,ampere,in,deg,mi,ft,MWh,in/day,cfs,langley/min,MW,in-hg,langley,mph,rpm,ac-ft,F,sec,JTU,FNU,volt,ft3,su
    /// </summary>
    public string Units { get; set; }



    public DssPath Path { get; set; }

    public bool IsRegular()
    {
      return Path.IsRegular;
    }

    public bool IsRegular(DateTime[] times)
    {
      var temp = times;
      var td = temp[1] - temp[0];
      for (int i = 0; i < temp.Length; i++)
      {
        if (i == 0)
          continue;
        else if (i == temp.Length - 1)
          break;
        else
        {
          if (temp[i + 1] - temp[i] == td) // check if time difference is the same throughout list
            continue;
          else
            return false;
        }
      }
      return true;
    }

    /// <summary>
    /// INST-VAL, INST-CUM, PER-CUM, PER-AVER
    /// </summary>
    public string DataType { get; set; }
    public int Count { get { return Values.Length; } }

    public LocationInformation LocationInformation { get; set; }
    public bool HasQuality { get { return Qualities != null; } }

    public string ProgramName { get; internal set; }

    public TimeSeriesPoint this[int i]
    {
      get
      {
        var rval = new TimeSeriesPoint(Times[i], Values[i]);
        return rval;
      }
      set
      {
        Values[i] = value.Value;
        Times[i] = value.DateTime;
        if (HasQuality)
        {
          Qualities[i] = value.IntQuality;
        }
      }
    }

    public void SetQuality(int[] quality)
    {
      if (quality.Length != Values.Length)
        throw new Exception("Size of quality array doesn't match value array.");
      Qualities = quality;
    }

    internal void Append(TimeSeries other)
    {
      //Doing this for the time being until the c version of DSS handles condensed paths. Other will append values and times.
      List<double> newValues = new List<double>(Values.Length + other.Values.Length);
      newValues.AddRange(Values);
      newValues.AddRange(other.Values);
      Values = newValues.ToArray();

      List<DateTime> newTimes = new List<DateTime>(Times.Length + other.Times.Length);
      newTimes.AddRange(Times);
      newTimes.AddRange(other.Times);
      Times = newTimes.ToArray();

      if (other.HasQuality)
      {
        List<int> newQualities = new List<int>(Qualities.Length + other.Qualities.Length);
        newQualities.AddRange(Qualities);
        newQualities.AddRange(other.Qualities);
        Qualities = newQualities.ToArray();
      }
    }

    public DataTable ToDataTable(bool ShowIndex = true, bool ShowQuality = true)
    {
      var dt = new DataTable();
      var cols = new List<DataColumn>();
      if (ShowIndex)
        cols.Add(new DataColumn("Index", typeof(int)));
      cols.Add(new DataColumn("DateTime", typeof(DateTime)));
      cols.Add(new DataColumn("Value", typeof(double)));
      if (ShowQuality)
        cols.Add(new DataColumn("Quality", typeof(string)));
      dt.Columns.AddRange(cols.ToArray());

      for (int i = 0; i < Count; i++)
        if (ShowIndex && ShowQuality)
          dt.Rows.Add(i + 1, Times[i], Values[i], Qualities == null ? "" : Quality.ToString(Qualities[i]));
        else if (ShowIndex && !ShowQuality)
          dt.Rows.Add(i + 1, Times[i], Values[i]);
        else if (!ShowIndex && ShowQuality)
          dt.Rows.Add(Times[i], Values[i], Qualities == null ? "" : Quality.ToString(Qualities[i]));
        else
          dt.Rows.Add(Times[i], Values[i]);

      return dt;
    }

    public List<TimeSeriesPoint> ToTimeSeriesPoints()
    {
      List<TimeSeriesPoint> list = new List<TimeSeriesPoint>();
      if (Qualities != null)
      {
        for (int i = 0; i < Count; i++)
          list.Add(new TimeSeriesPoint(Times[i], Values[i], Qualities[i]));
      }
      else
      {
        for (int i = 0; i < Count; i++)
          list.Add(new TimeSeriesPoint(Times[i], Values[i]));
      }
     

      return list;
    }

    private TimeSeries NoDataCompression()
    {
      var ts = new TimeSeries();
      ts.Init(this.Path, new double[] { }, this.StartDateTime, this.Units, this.DataType);
      ts.LocationInformation = this.LocationInformation;
      var newValues = new List<double>();
      var newTimes = new List<DateTime>();

      int NoneCount = 0;
      for (int i = 0; i < Count; i++)
      {
        if (!DssReader.IsValid(Values[i])) // If current value is no data
          NoneCount++;
        if ((i + 1 != Count && DssReader.IsValid(Values[i])) || (i == Count - 1 && !DssReader.IsValid(Values[i]))) // Check if next value is no data or if last value is no data
          NoneCount = 0;
        if (NoneCount >= 2) // If current no data count is at 2 or above, skip value
          continue;
        newValues.Add(Values[i]);
        newTimes.Add(Times[i]);
      }

      ts.Values = newValues.ToArray();
      ts.Times = newTimes.ToArray();
      ts._compressed = true;
      return ts;
    }

    private TimeSeries AnyValueCompression()
    {
      var ts = new TimeSeries();
      ts.Init(this.Path, new double[] { }, this.StartDateTime, this.Units, this.DataType);
      ts.LocationInformation = this.LocationInformation;
      var newValues = new List<double>();
      var newTimes = new List<DateTime>();

      int AnyValueCount = 0;
      double value = Values[0];
      for (int i = 1; i < Count; i++)
      {
        if (Values[i] == value) // If current value is repeated
          AnyValueCount++;
        if ((i + 1 != Count && Values[i + 1] != value) || (i == Count - 1 && Values[i] == value)) // Check if next value or last value isnt repeated
        {
          AnyValueCount = 0;
          if (i != Count - 1) {value = Values[i + 1];}
        }
        if (AnyValueCount >= 2) // If current no data/zero count is at 2 or above, skip value
          continue;
        newValues.Add(Values[i]);
        newTimes.Add(Times[i]);
      }

      ts.Values = newValues.ToArray();
      ts.Times = newTimes.ToArray();
      ts._compressed = true;
      return ts;
    }

    private TimeSeries ZeroAndNoDataCompression()
    {
      var ts = new TimeSeries();
      ts.Init(this.Path, new double[] { }, this.StartDateTime, this.Units, this.DataType);
      ts.LocationInformation = this.LocationInformation;
      var newValues = new List<double>();
      var newTimes = new List<DateTime>();

      int ZeroAndNoneCount = 0;
      for (int i = 0; i < Count; i++)
      {
        if (Values[i] == 0 || !DssReader.IsValid(Values[i])) // If current value is no data/zero
          ZeroAndNoneCount++;
        if ((i + 1 != Count && (Values[i + 1] != 0 && DssReader.IsValid(Values[i]))) || (i == Count - 1 && (Values[i] == 0 || !DssReader.IsValid(Values[i])))) // Check if next value is no data or if last value is no data
          ZeroAndNoneCount = 0;
        if (ZeroAndNoneCount >= 2) // If current no data/zero count is at 2 or above, skip value
          continue;
        newValues.Add(Values[i]);
        newTimes.Add(Times[i]);
      }

      ts.Values = newValues.ToArray();
      ts.Times = newTimes.ToArray();
      ts._compressed = true;
      return ts;
    }

    internal TimeSeries Compress(ConsecutiveValueCompression compressionType = ConsecutiveValueCompression.None)
    {
      if (compressionType == TimeWindow.ConsecutiveValueCompression.ZeroAndNoData)
        return ZeroAndNoDataCompression();
      else if (compressionType == TimeWindow.ConsecutiveValueCompression.NoData)
        return NoDataCompression();
      else if (compressionType == ConsecutiveValueCompression.AnyValue)
        return AnyValueCompression();
      else
        return this;
    }

    /// <summary>
    /// Removes missing or invalid values at the beginning and end of TimeSeries.
    /// </summary>
    public void Trim()
    {

      int first = Array.FindIndex(Values, val => DssReader.IsValid(val));

      if( first <0)
      {
        Clear(); // no value data, clear all
        return;
      }
      int last = Array.FindLastIndex(Values, val => DssReader.IsValid(val));


      int newSize = last - first + 1;
      double[] tempValues = new double[newSize];
      DateTime[] tempDates = new DateTime[newSize];
      int[] tempQuality = new int[newSize];

      Array.Copy(Values, first, tempValues, 0, newSize);
      Array.Copy(Times, first, tempDates, 0, newSize);
      if (HasQuality)
        Array.Copy(Qualities, first, tempQuality, 0, newSize);

      Values = tempValues;
      Times = tempDates;
      if (HasQuality)
        Qualities = tempQuality;
    }

    /// <summary>
    /// Clear all values and times from the TimeSeries instance
    /// </summary>
    /// <exception cref="NotImplementedException"></exception>
    private void Clear()
    {
      Values = new double[0];
      Times = new DateTime[0];
      if (HasQuality)
        Qualities = new int[0];
    }

    public TimeSpan TimeSpanInterval()
    {
      if (IsRegular(Times))
      {
        if (Times.Length < 2)
          return new TimeSpan();
        return Times[1] - Times[0];
      }
      return new TimeSpan();
    }

    
  }
}
