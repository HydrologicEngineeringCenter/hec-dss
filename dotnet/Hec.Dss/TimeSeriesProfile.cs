using System;
using System.Data;

namespace Hec.Dss
{
  public class TimeSeriesProfile
  {
    public TimeSeriesProfile()
    {
      Units = "";
      DataType = "";
      ColumnUnits = "";
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
        for (int j = 0; j < Values.GetLength(0); j++)
        {
          Console.WriteLine(Times[i].ToString() + " " + Values[i,j]);
        }
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
        if (Times == null || Times.Length == 0)
          throw new NullReferenceException("Error StartDateTime is not defined");
        else return Times[0];
      }
      set
      {
        if (Times == null)
          Times = new DateTime[1] { value };
        else if (Times.Length > 0)
        {
          Times[0] = value;
        }
      }
    }

    public double[,] Values { get; set; }
    public DateTime[] Times { get; set; }

    /// <summary>
    /// %,ft2,ppm,umho/cm, unit,$,ampere,in,deg,mi,ft,MWh,in/day,cfs,langley/min,MW,in-hg,langley,mph,rpm,ac-ft,F,sec,JTU,FNU,volt,ft3,su
    /// </summary>
    public string Units { get; set; }

    public string ColumnUnits { get; set; }


    public DssPath Path { get; set; }

    /// <summary>
    /// INST-VAL, INST-CUM, PER-CUM, PER-AVER
    /// </summary>
    public string DataType { get; set; }
    public int Count { get { return Values.GetLength(0); } }

    public LocationInformation LocationInformation { get; set; }
    public double[] ColumnValues { get; set; }

    public TimeSeriesPoint this[int i,int j]
    {
      get
      {
        var rval = new TimeSeriesPoint(Times[i], Values[i,j]);
        return rval;
      }
    }

    public DataTable ToDataTable()
    {
      var dt = new DataTable();
      dt.Columns.Add("DateTime", typeof(DateTime));
      for (int i = 0; i<ColumnValues.Length; i++)
      {
        dt.Columns.Add(ColumnValues[i].ToString("F2"), typeof(double));
      }
      for (int i = 0; i < Count; i++)
      {
        var r = dt.NewRow();
        dt.Rows.Add(r);
        r[0] = Times[i];
        for (int j = 0; j < ColumnValues.Length; j++)
        {
          r[j + 1] = Values[i,j];
        }
      }

      return dt;
    }
  }
}
