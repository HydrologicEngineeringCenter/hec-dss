using System;
using System.Collections.Generic;
using static Hec.Dss.Quality;

namespace Hec.Dss
{
  public class TimeSeriesPoint
  {
    public TimeSeriesPoint(DateTime dateTime, double value)
    {
      DateTime = dateTime;
      Value = value;
    }

    public TimeSeriesPoint(DateTime dateTime, double value, int quality)
    {
      DateTime = dateTime;
      Value = value;
      this.quality = quality;
    }

    public TimeSeriesPoint(DateTime dateTime, double value, BaseQualityFlags quality)
    {
      DateTime = dateTime;
      Value = value;
      this.quality = (int)quality;
    }

    public override string ToString()
    {
      Time.DateTimeToHecDateTime(this.DateTime, out string d, out string t);
      return d + " " + t + ", " + this.Value.ToString("F2");
    }

    public DateTime DateTime { get; set; }

    public double Value { get; set; }

    public void SetQuality(BaseQualityFlags flag)
    {
      if (quality == -1)
      {
        quality = 0;
        quality |= (int)flag;
      }
      quality |= (int)flag;
    }
    
    public void SetQuality(int flag)
    {
      if (quality == -1)
      {
        quality = 0;
        quality |= flag;
      }
      quality |= flag;
    }

    public void ClearQuality(BaseQualityFlags flag)
    {
      if (quality == -1)
        return;
      quality ^= (int)flag;
    }
    
    public void ClearQuality(int flag)
    {
      if (quality == -1)
        return;
      quality ^= flag;
    }

    public void ClearAllQualities()
    {
      quality = 0;
    }

    public string Quality { get { return quality == -1 ? "" : Hec.Dss.Quality.ToString(quality); } }
    public int IntQuality { get { return quality; } }
    private int quality = -1;
    public List<Quality> QualityStats { get { return QualityStats(this);} }
  }
}
