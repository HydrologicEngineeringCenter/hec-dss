using System;
using System.Collections.Generic;
using Hec.Dss.Native;

namespace Hec.Dss
{
  public class DssWriter : DssReader
  {
    static DssWriter() => Assembly.Initialize();

    public DssWriter(string filename, MethodID messageMethod = MethodID.MESS_METHOD_GENERAL_ID, 
      LevelID messageLevel = LevelID.MESS_LEVEL_GENERAL) : base(filename, messageMethod, messageLevel)
    {

    }

    public DssWriter(string filename, int version, MethodID messageMethod = MethodID.MESS_METHOD_GENERAL_ID,
      LevelID messageLevel = LevelID.MESS_LEVEL_GENERAL) : base(filename, version, messageMethod, messageLevel)
    {

    }


    public int Write(TimeSeriesProfile profile, bool saveAsFloat = false)
    {
      if (profile.Values.Length <= 0)
        return -1;

      int numCols = profile.ColumnValues.Length;
      int numRows = profile.Values.GetLength(0);
      Time.DateTimeToHecDateTime(profile.StartDateTime, out string startDate, out string startTime);
      var tss = PInvoke.NativeTsNewTimes(profile.Path.FullPath, startDate, startTime, "", "");
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

      return PInvoke.ZTsStore(ifltab, tss, storageFlagOverwrite);

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
      int alwaysReplace = 0;
      float[] floats = null;
      if (saveAsFloat)
        floats = Array.ConvertAll(ts.Values, item => (float)item);
      if (ts.IsRegular())
      {// store as Regular times series
        if (saveAsFloat)
          rval = StoreTimeSeriesRegular<float>(ts.Path.FullPath, floats, ts.Qualities, alwaysReplace, ts.StartDateTime, ts.Units, ts.DataType);
        else
          rval = StoreTimeSeriesRegular<double>(ts.Path.FullPath, ts.Values, ts.Qualities, alwaysReplace, ts.StartDateTime, ts.Units, ts.DataType);

      }
      else
      {// irregular 

        int timeGranularitySeconds = GetIrregularTimeGranularity(ts);

        int julianBaseDate = GetIrregularJulianBaseDate(ts, timeGranularitySeconds);
        string startBaseDate = Time.JulianToHecDate(julianBaseDate);
        int[] times = Time.ConvertDatesToHecInteger(ts.Times, julianBaseDate, timeGranularitySeconds);
        if (saveAsFloat)
          rval = StoreTimeSeriesIrregular<float>(ts.Path.FullPath, floats, ts.Qualities, times, alwaysReplace, timeGranularitySeconds, startBaseDate, ts.Units, ts.DataType);
        else
          rval = StoreTimeSeriesIrregular<double>(ts.Path.FullPath, ts.Values, ts.Qualities, times, alwaysReplace, timeGranularitySeconds, startBaseDate, ts.Units, ts.DataType);
      }

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

    public void StoreLocation(string dssPath, LocationInformation loc, bool overwrite = false)
    {
      ZStructLocationWrapper wrapper = DSS.ZStructLocationNew(dssPath);
      wrapper.CoordinateID = loc.CoordinateID;
      wrapper.CoordinateSystem = (int)loc.CoordinateSystem;
      wrapper.HorizontalDatum = loc.HorizontalDatum;
      wrapper.HorizontalUnits = loc.HorizontalUnits;
      wrapper.PathName = dssPath;
      wrapper.Supplemental = loc.Supplemental;
      wrapper.TimeZoneName = loc.TimeZoneName;
      wrapper.VerticalDatum = loc.VerticalDatum;
      wrapper.VerticalUnits = loc.VerticalUnits;
      wrapper.XOrdinate = loc.XOrdinate;
      wrapper.YOrdinate = loc.YOrdinate;
      wrapper.ZOrdinate = loc.ZOrdiante;
      DSS.ZLocationStore(ref ifltab, ref wrapper, overwrite ? 1 : 0);

    }


    /// <summary>
    /// Store regular time series values into the DSS file
    /// </summary>
    /// <typeparam name="T">Must be float or double.  Anything else causes exception.</typeparam>
    /// <param name="pathName">path name to the time series data</param>
    /// <param name="storageFlag">0:Always replace data. 1:Only replace missing data. 2:Write regardless, even if all missing data (write a missing record) 3:If a record is all missing, do not write it, and delete it from disk if it exists. 4:Do not allow a missing input data to replace a valid data.</param>
    /// <param name="startDate">The starting date of the time series.  Must be character Ex: "09Jan1982" or Julian days since 01Jan1900</param>
    /// <param name="startTime">The time in military time format.</param>
    /// <param name="units">%,ft2,ppm,umho/cm, unit,$,ampere,in,deg,mi,ft,MWh,in/day,cfs,langley/min,MW,in-hg,langley,mph,rpm,ac-ft,F,sec,JTU,FNU,volt,ft3,su</param>
    /// <param name="type">INST-VAL, INST-CUM, PER-CUM, PER-AVER</param>
    private int StoreTimeSeriesRegular<T>(string pathName, T[] values, int[] qualities, int storageFlag, string startDate, string startTime, string units, string type) where T : struct
    {
      Type listType = typeof(T);
      if (listType == typeof(double))
      {
        double[] dArray = values as double[];
        NativeTimeSeriesWrapper tss = PInvoke.NativeTsNewRegDoubles(pathName, dArray, startDate, startTime, units, type);
        if (qualities != null)
        {
          tss.QualityElementSize = 1;
          tss.Quality = qualities;
        }
        return PInvoke.ZTsStore(ifltab, tss, storageFlag);
      }
      else if (listType == typeof(float))
      {
        float[] fArray = values as float[];
        NativeTimeSeriesWrapper tss = PInvoke.NativeTsNewRegFloats(pathName, fArray, startDate, startTime, units, type);
        if (qualities != null)
        {
          tss.QualityElementSize = 1;
          tss.Quality = qualities;
        }
        return PInvoke.ZTsStore(ifltab, tss, storageFlag);
      }
      else
        throw new Exception("Cannot store values of type " + listType.Name + " for time series.  Only accepts double and float");
    }



    /// <summary>
    /// Store regular time series values into the DSS file
    /// </summary>
    /// <typeparam name="T">Must be float or double.  Anything else causes exception.</typeparam>
    /// <param name="pathName">path name to the time series data</param>
    /// <param name="storageFlag">0:Always replace data. 1:Only replace missing data. 2:Write regardless, even if all missing data (write a missing record) 3:If a record is all missing, do not write it, and delete it from disk if it exists. 4:Do not allow a missing input data to replace a valid data.</param>
    /// <param name="startDateTime">The start date and time of the time series.</param>
    /// <param name="units">%,ft2,ppm,umho/cm, unit,$,ampere,in,deg,mi,ft,MWh,in/day,cfs,langley/min,MW,in-hg,langley,mph,rpm,ac-ft,F,sec,JTU,FNU,volt,ft3,su</param>
    /// <param name="type">INST-VAL, INST-CUM, PER-CUM, PER-AVER</param>
    private int StoreTimeSeriesRegular<T>(string pathName, T[] values, int[] qualities, int storageFlag, DateTime startDateTime, string units, string type) where T : struct
    {
      Time.DateTimeToHecDateTime(startDateTime, out string startDate, out string startTime);
      return StoreTimeSeriesRegular(pathName, values, qualities, storageFlag, startDate, startTime, units, type);
    }



    /// <summary>
    /// Store irregular time series values into the DSS file 
    /// </summary>
    /// <typeparam name="T">Must be float or double.  Anything else causes exception.</typeparam>
    /// <param name="pathName">path name to the time series data</param>
    /// <param name="itimes">times must be minutes or seconds from the “base date”, which is used primarily so that integers in the time array don’t exceed the maximum int value.  The base date is often the date of the start of the data, or blank, for standard HEC Julian dates (days since 31Dec1899).</param>
    /// <param name="storageFlag">0:Merge.  If a value at a time does not exist, that is inserted.  If it does exist, it is replaced. 1:Replace.The data read from the start to end is removed and this data replaces it.</param>
    /// <param name="timeGranularitySeconds">1 for second granularity and 60 for minute granularity.</param>
    /// <param name="startDateBase">The base date is often the date of the start of the data.</param>
    /// <param name="units">%,ft2,ppm,umho/cm, unit,$,ampere,in,deg,mi,ft,MWh,in/day,cfs,langley/min,MW,in-hg,langley,mph,rpm,ac-ft,F,sec,JTU,FNU,volt,ft3,su</param>
    /// <param name="type">INST-VAL, INST-CUM, PER-CUM, PER-AVER</param>
    private int StoreTimeSeriesIrregular<T>(string pathName, T[] values, int[] qualities, int[] itimes, int storageFlag, int timeGranularitySeconds, string startDateBase, string units, string type) where T : struct
    {
      
      Type listType = typeof(T);
      if (listType == typeof(double))
      {
        double[] dArray = values as double[];
        NativeTimeSeriesWrapper tss = PInvoke.NativeTsNewIrregDoubles(pathName, dArray, itimes, timeGranularitySeconds, startDateBase, units, type);
        if (qualities != null)
        {
          tss.QualityElementSize = 1;
          tss.Quality = qualities;
        }
        return PInvoke.ZTsStore(ifltab, tss, storageFlag);
      }
      else if (listType == typeof(float))
      {
        float[] fArray = values as float[];
        NativeTimeSeriesWrapper tss = PInvoke.NativeTsNewIrregFloats(pathName, fArray, itimes, timeGranularitySeconds, startDateBase, units, type);
        if (qualities != null)
        {
          tss.QualityElementSize = 1;
          tss.Quality = qualities;
        }
        return PInvoke.ZTsStore(ifltab, tss, storageFlag);
      }
      else
        throw new Exception("Cannot store values of type " + listType.Name + " for time series.  Only accepts double and float");
    }

    //IF we want multiple curves, we have to change values to be a double array and make that work.
    /// <summary>
    /// Store paired data into DSS file
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <param name="pathName">Must be float or double.  Anything else causes exception.</param>
    /// <param name="ordinates">independent variable</param>
    /// <param name="values">The array must contain numberCurves * numberOrdinates values. Ex: an array with 5 curves and 200 ordinates would be T[5][200]</param>
    /// <param name="storageFlag"></param>
    /// <param name="unitsIndependent"></param>
    /// <param name="typeIndependent"></param>
    /// <param name="unitsDependent"></param>
    /// <param name="typeDependent"></param>
    public void StorePairedData<T>(string pathName, T[] ordinates, T[] values, int storageFlag, int numberCurves, string unitsIndependent, string typeIndependent, string unitsDependent, string typeDependent) where T : struct
    {
      Type listType = typeof(T);
      if (listType == typeof(double))
      {
        double[] dValues = values as double[];
        double[] dOrdinates = ordinates as double[];
        ZStructPairedDataWrapper pds = DSS.ZStructPdNewDoubles(pathName, ref dOrdinates, ref dValues, ordinates.Length, numberCurves, unitsIndependent, typeIndependent, unitsDependent, typeDependent);
        DSS.ZpdStore(ref ifltab, ref pds, storageFlag);
      }
      else if (listType == typeof(float))
      {
        float[] fValues = values as float[];
        float[] fOrdinates = ordinates as float[];
        ZStructPairedDataWrapper pds = DSS.ZStructPdNewFloats(pathName, ref fOrdinates, ref fValues, ordinates.Length, numberCurves, unitsIndependent, typeIndependent, unitsDependent, typeDependent);
        DSS.ZpdStore(ref ifltab, ref pds, storageFlag);
      }
      else
        throw new Exception("Cannot store values of type " + listType.Name + " for paired data.  Only accepts double and float");
    }

    //IF we want multiple curves, we have to change values to be a double array and make that work.
    /// <summary>
    /// Store paired data into DSS file
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <param name="path">Must be float or double.  Anything else causes exception.</param>
    /// <param name="ordinates">independent variable</param>
    /// <param name="values">The array must contain numberCurves * numberOrdinates values. Ex: an array with 5 curves and 200 ordinates would be T[5][200]</param>
    /// <param name="storageFlag"></param>
    /// <param name="unitsIndependent"></param>
    /// <param name="typeIndependent"></param>
    /// <param name="unitsDependent"></param>
    /// <param name="typeDependent"></param>
    public void StorePairedData<T>(DssPath path, T[] ordinates, T[] values, int storageFlag, int numberCurves, string unitsIndependent, string typeIndependent, string unitsDependent, string typeDependent) where T : struct
    {
      StorePairedData<T>(path.FullPath, ordinates, values, storageFlag, numberCurves, unitsIndependent, typeIndependent, unitsDependent, typeDependent);
    }

    /// <summary>
    /// Stores a grid in DSS file
    /// </summary>
    /// <param name="pathName">The pathname to store the grid</param>
    /// <param name="grid">the grid you want to store</param>
    public void StoreGrid(string pathName, Grid grid)
    {
      if (versionNumber == 6)
      {
        throw new Exception("Cannot write grids to DSS version 6 files");
      }
      ZStructSpatialGridWrapper gs;
      if (grid.DSSObj != null)
      { //this grid was read in before
        gs = grid.DSSObj;
        gs.PathName = pathName;
      }
      else
      {
        gs = DSS.ZStructSpatialGridNew(pathName);
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
      }
      DSS.ZSpatialGridStore(ref ifltab, ref gs);
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
    /// Note that the parameters have no effect on Version 6 files.
    /// </summary>
    /// <param name="OnlyIfNeeded">Set to true to squeeze only if the file needs to be squeezed Set to 0 to force a squeeze.  Has no effect on version 6 files</param>
    /// <param name="InPlaceSqueeze">Set to true to force an in-place squeeze, retaining ownership, etc.  
    /// Copies back so the same file is used.This takes twice as long as a normal squeeze.
    /// Set to 0 for a normal squeeze, where a file rename takes place(preferred).  Has no effect on version 6 files</param>
    /// <returns>True if successful, false if unsuccessful</returns>
    public bool Squeeze(bool OnlyIfNeeded, bool InPlaceSqueeze)
    {
      if (versionNumber == 7)
      {
        int status = DSS.zSqueeze7(ref ifltab, OnlyIfNeeded ? 1 : 0, InPlaceSqueeze ? 1 : 0);
        if (status < 0)
        {
          return false;
        }
        return true;
      }
      else
      {
        int status = DSS.ZSqueeze(filename);
        if (status < 0)
        {
          return false;
        }
        else
          return true;
      }

    }

    public void DeleteRecord(string pathName)
    {
      DSS.ZDelete(ref ifltab, pathName);
    }

    public void DeleteRecord(DssPath path)
    {
      DeleteRecord(path.FullPath);
    }

    public void Write(PairedData pd)
    {
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
    }

  }
}
