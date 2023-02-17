using System;
using System.Collections.Generic;
using System.Linq;

namespace Hec.Dss
{
  public class TimeSeriesUtility
  {

    /// <summary>Get the plot representation of this time series.</summary>
    /// <param name="x">Converted times on success, otherwise Times as they are already.</param>
    /// <param name="y">Converted values on success, otherwise Values as they are already.</param>
    /// <returns>True if successful.  False if failure or don't know how to convert instant values.</returns>
    public static bool TryConvertToGraphable(TimeSeries ts, out DateTime[] x, out double[] y)
    {
      var Times = ts.Times;
      var Values = ts.Values;

      if (string.IsNullOrEmpty(ts.DataType) || ts.DataType.ToUpperInvariant() == "INST-VAL")
      {
        //no conversion needed
        x = ts.Times;
        y = ts.Values;

        return true;
      }
      else if (ts.DataType.ToUpperInvariant() == "PER-AVER")
      {
        //Get the period by reading the E part.  E part may not have a time span though.
        var timeSpan = ts.Path.EPartAsTimeSpan;

        List<DateTime> dts = new List<DateTime>();
        List<double> vals = new List<double>();

        if (timeSpan != default(TimeSpan))
        {
          //regular time series. set first dt to a period before the first value
          dts.Add(Times[0].Subtract(timeSpan));
          vals.Add(Values[0]);
        }

        for (int i = 0; i < Times.Length - 1; i++)
        {
          dts.Add(Times[i]);
          vals.Add(Values[i]);
          dts.Add(Times[i]);
          vals.Add(Values[i + 1]);
        }

        dts.Add(Times.Last());
        vals.Add(Values.Last());

        x = dts.ToArray();
        y = vals.ToArray();
        return true;
      }

      //Don't know how to convert, so return value as they are.
      x = Times;
      y = Values;
      return false;
    }

    /// <param name="instantValues">Converted values on success, otherwise Values as they are already.</param>
    /// <returns>True if successful.  False if failure or don't know how to convert instant values.</returns>
    public static bool TryConvertToInstantValue(TimeSeries ts,out DateTime[] instantTimes, out double[] instantValues)
    {
      var Values = ts.Values;
      var Times = ts.Times;

      if (string.IsNullOrEmpty(ts.DataType) || ts.DataType.ToUpperInvariant() == "INST-VAL")
      {
        //no conversion needed
        instantValues = Values;
        instantTimes = Times;
        return true;
      }

      if (ts.DataType.ToUpperInvariant() == "PER-AVER")
      {
        //take middle value at each period, using graphable values
        DateTime[] graphableTimes = null;
        double[] graphableValues = null;
        if (TryConvertToGraphable(ts,out graphableTimes, out graphableValues))
        {
          List<DateTime> dts = new List<DateTime>();
          List<double> vals = new List<double>();

          dts.Add(graphableTimes[0]);
          vals.Add(graphableValues[0]);

          for (int i = 0; i < graphableTimes.Length - 1; i += 2)
          {
            //this may overflow.. but I doubt it
            var averageTicks = (graphableTimes[i].Ticks + graphableTimes[i + 1].Ticks) / 2.0;
            dts.Add(new DateTime((long)averageTicks));
            vals.Add(graphableValues[i]);
          }

          dts.Add(graphableTimes.Last());
          vals.Add(graphableValues.Last());

          instantTimes = dts.ToArray();
          instantValues = vals.ToArray();

          return true;
        }

      }

      //either unsuccessful or we don't know how to do it
      instantTimes = Times;
      instantValues = Values;
      return false;
    }


  }
}
