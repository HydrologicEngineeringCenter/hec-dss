using System;
using System.Collections.Generic;

namespace Hec.Dss
{
  public class DssWriter : DssReader
  {
    public DssWriter(string filename, int messageLevel =3) : base(filename,  messageLevel)
    {

    }

    public int Write(TimeSeriesProfile profile, bool saveAsFloat = false)
    {
      throw new NotImplementedException("TimeSeriesProfile profile");
      if (profile.Values.Length <= 0)
        return -1;

      int numCols = profile.ColumnValues.Length;
      int numRows = profile.Values.GetLength(0);
      Time.DateTimeToHecDateTime(profile.StartDateTime, out string startDate, out string startTime);
/*      var tss = DSS.ZStructTsNewTimes(profile.Path.FullPath, startDate, startTime, "", "");
      tss.UnitsProfileValues = profile.Units;
      tss.UnitsProfileDepths = profile.ColumnUnits;
      tss.NumberValues = numRows;
      tss.ProfileDepthsNumber = numCols;
      int storageFlagOverwrite = 0;


      if (saveAsFloat)
      {
        float[] columnValues = Array.ConvertAll(profile.ColumnValues, item => (float)item);
        var fArray = MatrixToFloatArray(profile.Values, numCols, numRows);
        tss.FloatProfileDepths = columnValues;
        tss.FloatProfileValues = fArray;
      }
      else
      { // doubles

        double[] d = MatrixToDoubleArray(profile.Values, numCols, numRows);
        tss.DoubleProfileDepths = profile.ColumnValues;
        tss.DoubleProfileValues = d;
      }

      return DSS.ZTsStore(ref ifltab, ref tss, storageFlagOverwrite);
*/
    }

    private static float[] MatrixToFloatArray(double[,] values, int numCols, int numRows)
    {
      float[] fArray = new float[numCols * numRows];
      int pos = 0;
      for (int r = 0; r < numRows; r++)
      {
        for (int c = 0; c < numCols; c++)
        {
          // int pos = r* numRows + c ;
          if (pos < fArray.Length)
            fArray[pos] = (float)values[r,c];
          pos++;
        }
      }

      return fArray;
    }
    private static double[] MatrixToDoubleArray(double[,] values, int numCols, int numRows)
    {
      double[] fArray = new double[numCols * numRows];
      int pos = 0;
      for (int r = 0; r < numRows; r++)
      {
        for (int c = 0; c < numCols; c++)
        {
          // int pos = r* numRows + c ;
          if (pos < fArray.Length)
            fArray[pos] = values[r,c];
          pos++;
        }
      }

      return fArray;
    }

    /// <summary>
    /// Saves Time Series data
    /// 
    /// The path definition looks like /Basin/Location/Parameter/BlockDate/Interval/Version
    /// The interval determines if it is stored as regular or irregular.
    /// ('1Day', '1Hour', '1Second', etc.... ) store as Regular
    /// ('~1Day', 'IR-Day', 'IR-Month', etc...) store as Irregular
    /// </summary>
    /// <param name="ts">DSSTimeSeries object with data and properties to save</param>
    /// <param name="saveAsFloat">when true saves values to disk as floats instead of doubles</param>
    /// <returns></returns>
    public int Write(TimeSeries ts, bool saveAsFloat = false)
    {
      if (ts.Count == 0)
        return -1;
      if (ts.Path == null)
        throw new Exception("Path was not specified");

      int rval = -1;
      var units = new ByteString(ts.Units);
      var type = new ByteString(ts.DataType);

      if (ts.IsRegular())
      {// store as Regular times series
        Time.DateTimeToHecDateTime(ts.StartDateTime, out string date, out string time);
        int qualityLength = 0;
        if( ts.Qualities != null )
           qualityLength = ts.Qualities.Length;
        rval = DssNative.hec_dss_tsStoreRegular(dss, ts.Path.FullPath,date, time, ts.Values,
                   ts.Values.Length,ts.Qualities,qualityLength, saveAsFloat ? 1 : 0, units.Data, type.Data);

      }
      else
      {// irregular 
        int timeGranularitySeconds = GetIrregularTimeGranularity(ts);
        int julianBaseDate = GetIrregularJulianBaseDate(ts, timeGranularitySeconds);
        string startBaseDate = Time.JulianToHecDate(julianBaseDate);
        int[] times = Time.ConvertDatesToHecInteger(ts.Times, julianBaseDate, timeGranularitySeconds);

        DssNative.hec_dss_tsStoreIregular(dss, ts.Path.FullPath, startBaseDate, times,timeGranularitySeconds,
           ts.Values, ts.Values.Length, ts.Qualities, ts.Qualities.Length, saveAsFloat ? 1 : 0, units.Data, type.Data);
      }
      if (ts.LocationInformation != null)
        StoreLocation(ts.Path.FullPath, ts.LocationInformation, false);

      return rval;
    }

    /// <summary>
    /// Determine julian Base date
    /// usually this is zero, but to prevent integer overflow 
    /// </summary>
    /// <param name="ts"></param>
    /// <returns></returns>
    private int GetIrregularJulianBaseDate(TimeSeries ts, int timeGranularitySeconds)
    {
      if (timeGranularitySeconds == 1)
        return Time.DateToJulian(ts.StartDateTime);
      return 0;
    }

    private int GetIrregularTimeGranularity(TimeSeries ts)
    {
      var rval = 60;

      if (ts.Count < 2) // no data or just one point
        return rval;

      // if data points increment by less than a minute, use second granularity
      for (int i = 1; i < ts.Count; i++)
      {
        var t1 = ts[i - 1].DateTime;
        var t2 = ts[i].DateTime;
        var sec = t2.Subtract(t1).TotalSeconds;
        if (sec < 60)
        {
          rval = 1;
          return rval;
        }

      }

      // is data span is more than 100 years..

      var t3 = ts[0].DateTime;
      var t4 = ts[ts.Count - 1].DateTime;
      var years = t4.Subtract(t3).TotalDays / 365.25;
      if (years > 100 && ts.Path.Epart.ToLower() == "ir-century")
        rval = 86400;   //int DAY_GRANULARITY = 86400;

      return rval;
    }

    public int StoreLocation(string dssPath, LocationInformation loc, bool overwrite = false)
    {

      var timeZoneName = new ByteString(loc.TimeZoneName);
      var supplemental = new ByteString(loc.Supplemental);
      int status = DssNative.hec_dss_locationStore(dss, dssPath, loc.XOrdinate, loc.YOrdinate, loc.ZOrdiante,
         (int)loc.CoordinateSystem, loc.CoordinateID, loc.HorizontalUnits, loc.HorizontalDatum,
         loc.VerticalUnits, loc.VerticalDatum, timeZoneName.Data, supplemental.Data, overwrite? 1:0);
      return status;
    }



    /// <summary>
    /// Stores a grid in DSS file
    /// </summary>
    /// <param name="pathName">The pathname to store the grid</param>
    /// <param name="grid">the grid you want to store</param>
    public void StoreGrid(string pathName, Grid grid)
    {
      
      /*
        gs.DataUnits = grid.DataUnits;
        gs.DataType = (int)grid.DataType;
        gs.LowerLeftCellX = grid.LowerLeftCellX;
        gs.LowerLeftCellY = grid.LowerLeftCellY;
        gs.NumberOfCellsX = grid.NumberOfCellsX;
        gs.NumberOfCellsY = grid.NumberOfCellsY;
        gs.CellSize = grid.CellSize;
        gs.MaxDataValue = grid.MaxDataValue;
        gs.MinDataValue = grid.MinDataValue;
        gs.MeanDataValue = grid.MeanDataValue;
        gs.NumberOfRanges = grid.NumberOfRanges;
        gs.RangeLimitTable = grid.RangeLimitTable;
        gs.NumberEqualOrExceedingRangeLimit = grid.NumberEqualOrExceedingRangeLimit;
      */
    
    }

    /// <summary>
    /// Stores a grid in DSS file
    /// </summary>
    /// <param name="path">The pathname to store the grid</param>
    /// <param name="grid">the grid you want to store</param>
    public void StoreGrid(DssPath path, Grid grid)
    {
      StoreGrid(path.FullPath, grid);
    }

    /// <summary>
    ///A squeeze rebuilds a file, which removes inactive space, 
    /// rebuilds internal tables and adjust table sizes to optimize data access.
    /// Squeezing uses a brute force approach, which will recover any data sets that may 
    /// have broken links(rare), usually from a crash or disk damage.
    /// This is similar to de-fragmenting your file system.
    /// Once a squeeze has been accomplished, deleted data cannot be recovered.
    /// </summary>  
    public static bool Squeeze(string filename)
    {
      int status = DssNative.hec_dss_squeeze(filename);
      return status == 0;
    }


    public void DeleteRecord(string path)
    {
      DssNative.hec_dss_delete(dss, path);
    }

    public void DeleteRecord(DssPath path)
    {
      DeleteRecord(path.FullPath);
    }

    public void Write(PairedData pd)
    {
      throw new NotImplementedException("NotU+0020supportedU+0020yet.");
      /*
      int storageFlag = 2; // store as doubles
      List<double> d = new List<double>();
      foreach (var col in pd.Values)
      {
        foreach (var val in col)
        {
          d.Add(val);
        }
      }
      double[] dValues = d.ToArray();
      double[] dOrdinates = pd.Ordinates;
      ZStructPairedDataWrapper pds = DSS.ZStructPdNewDoubles(pd.Path.FullPath, ref dOrdinates, ref dValues, pd.Ordinates.Length, pd.Values.Count, pd.UnitsIndependent, pd.TypeIndependent, pd.UnitsDependent, pd.TypeDependent);
      DSS.ZpdStore(ref ifltab, ref pds, storageFlag);
      */
    }

  }
}
