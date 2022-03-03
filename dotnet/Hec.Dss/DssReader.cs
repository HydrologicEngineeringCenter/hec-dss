using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using Hec.Dss.Native;

namespace Hec.Dss
{
  public class DssReader : IDisposable
  {
    static DssReader() => Assembly.Initialize();


    private const double UNDEFINED_DOUBLE = -3.402823466e+38;
    private const double UNDEFINED_DOUBLE_2 = -3.4028234663852886E+38;

    protected long[] ifltab = new long[250];
    protected string filename;


    protected int versionNumber;

    static List<DssReader> ActiveReaders;

    DssPathCollection _catalog = null;

    bool _collectionHasMetaData = false;

    public string Filename
    {
      get { return filename; }
      private set { filename = value; }
    }

    /// <summary>
    /// Empty constructor for DSSWRITER to inherit
    /// </summary>
    protected DssReader()
    {
    }

    private GCHandle _iflTabGC;
    /// <summary>
    /// Constructor for DSSREADER object
    /// </summary>
    /// <param name="filename">Location of DSS file</param>
    public DssReader(string filename, MethodID messageMethod = MethodID.MESS_METHOD_GENERAL_ID, LevelID messageLevel = LevelID.MESS_LEVEL_GENERAL)
    {
      GetDssFile(filename, messageMethod, messageLevel);
    }

    public DssReader(string filename, int version, MethodID messageMethod = MethodID.MESS_METHOD_GENERAL_ID, LevelID messageLevel = LevelID.MESS_LEVEL_GENERAL)
    {
      DssGlobals.SetDefaultVersion(version);
      GetDssFile(filename, messageMethod, messageLevel);
    }

    private void GetDssFile(string filename, MethodID messageMethod, LevelID messageLevel)
    {
      if (ActiveReaders == null)
      {
        ActiveReaders = new List<DssReader>();
      }
      ActiveReaders.Add(this);
      if (messageMethod != MethodID.MESS_METHOD_GENERAL_ID || messageLevel != LevelID.MESS_LEVEL_GENERAL)
      {
        DssGlobals.SetMessageLevel(messageMethod,messageLevel);
      }
      _iflTabGC = GCHandle.Alloc(ifltab, GCHandleType.Pinned);
      int status;
      this.filename = filename;
      status = DSS.ZOpen(ref ifltab, filename);
      versionNumber = DSS.ZGetVersion(ref ifltab);

      switch (status)
      {
        case 0:
          //no problem
          break;

        case -1:
          throw new Exception("Unable to create the DSS file.");

        case -2:
          throw new Exception("Unable to connect to the DSS file.");

        case -3:
          throw new Exception("Incompatible DSS version.");

        case -10:
          throw new Exception("No filename provided.");

        default:
          throw new Exception("Error opening DSS file.");
      }
    }

    /// <summary>
    /// Gives condensed path names of the DSS File.  Condensed means that for every path name that has equal everything but D part, merge it into one path. 
    /// Will always give the path names sorted.
    /// </summary>
    /// <param name="includeMetaData"includeRecordType>set to true to get record type, Units, datatype, and location info</param>
    /// <returns></returns>
    public DssPathCollection GetCatalog(bool includeMetaData = false)
    {
      if (_catalog == null)
      {
        _catalog = GetCatalogInternal(includeMetaData);
      }
      else if (_catalog != null && !_collectionHasMetaData && includeMetaData)
        GetMetaDataForCatalog(_catalog);

      return _catalog;

    }

    private DssPathCollection GetCatalogInternal(bool includeMetaData)
    {
      bool sorted = false;
      RecordType[] recordTypes = null;

      ZStructCatalogWrapper cat = DSS.zStructCatalogNew();
      int status = DSS.ZCatalog(ref ifltab, null, ref cat, sorted ? 1 : 0);
      if (status < 0)
        throw new Exception("ZCatalog reported a failure with retrieving DSS filenames");

      var paths = cat.PathNameList;
      if (cat.RecordType != null)
        recordTypes = ConvertToRecordType(cat.RecordType);

      var condensed = new DssPathCollection(filename, paths, recordTypes, condensed: true);

      if (includeMetaData)
        GetMetaDataForCatalog(condensed);

      return condensed;
    }

    private void GetMetaDataForCatalog(DssPathCollection collection)
    {
      if (!collection.HasRecordTypes) // recordTypes is null for V6
      {
        SetRecordType(collection);

      }

      HashSet<string> uDataUnits = new HashSet<string>();
      HashSet<string> uDataTypes = new HashSet<string>();
      HashSet<double> uXs = new HashSet<double>();
      HashSet<double> uYs = new HashSet<double>();
      HashSet<double> uZs = new HashSet<double>();
      foreach (var item in collection)
      {
        var loc = DSS.ZLocationRetrieve(ref ifltab, item.FullPath);
        item.XOrdinate = loc.XOrdinate;
        item.YOrdinate = loc.YOrdinate;
        item.ZOrdinate = loc.ZOrdinate;
        uXs.Add(loc.XOrdinate);
        uYs.Add(loc.YOrdinate);
        uZs.Add(loc.ZOrdinate);

        ReadHeaderInfo(item, collection, uDataUnits, uDataTypes);
      }
      List<string> dataUnits = uDataUnits.ToList();
      dataUnits.Sort();

      List<string> dataTypes = uDataTypes.ToList();
      dataTypes.Sort();

      List<double> xs = uXs.ToList();
      xs.Sort();

      List<double> ys = uYs.ToList();
      ys.Sort();

      List<double> zs = uZs.ToList();
      zs.Sort();

      collection.SetUniqueDataTypes(dataTypes);
      collection.SetUniqueDataUnits(dataUnits);
      collection.SetUniqueXs(xs);
      collection.SetUniqueYs(ys);
      collection.SetUniqueZs(zs);

      _collectionHasMetaData = true;
    }

    private static RecordType[] ConvertToRecordType(int[] recTypes)
    {
      var rval = new RecordType[recTypes.Length];
      for (int i = 0; i < recTypes.Length; i++)
      {
        rval[i] = RecordTypeFromInt(recTypes[i]);
      }
      return rval;
    }

    private static RecordType RecordTypeFromInt(int recType)
    {
      RecordType rval = RecordType.Unknown;



      if (recType >= 100 && recType < 110) // see zdataTypeDescriptions.h
        rval = RecordType.RegularTimeSeries;
      else if (recType >= 110 && recType < 200)
        rval = RecordType.IrregularTimeSeries;
      else if (recType >= 200 && recType < 300)
        rval = RecordType.PairedData;
      else if (recType >= 300 && recType < 400)
        rval = RecordType.Text;
      else if (recType >= 400 && recType < 450)
        rval = RecordType.Grid;
      else if (recType == 450)
        rval = RecordType.Tin;
      else if (recType == 20)
        rval = RecordType.LocationInfo;

      return rval;
    }


    internal void ReadHeaderInfo(DssPath item, DssPathCollection paths = null, HashSet<string> dataUnits = null, HashSet<string> dataTypes = null)
    {

      if (item.RecordType == RecordType.RegularTimeSeries || item.RecordType == RecordType.IrregularTimeSeries)
      {
        TimeSeries ts = GetEmptyTimeSeries(item);  // 0.1 second 
                                                   //var ts = GetTimeSeries(item.PathWithoutDate); // 1 second   CondencedCatalog7Extended/test
        if (ts != null)
        {
          if (dataUnits != null && !dataUnits.Contains(ts.Units))
            dataUnits.Add(ts.Units);
          if (dataTypes != null && !dataTypes.Contains(ts.DataType))
            dataTypes.Add(ts.DataType);

          if (paths != null && paths._isInterned)
          {
            if (dataUnits == null || dataTypes == null)
              throw new ArgumentException("The paths is interned, but the dataUnits or the dataType hash set is null.  Neither can be null.");

            if (ts.Units == null)
              ts.Units = "";

            if (ts.DataType == null)
              ts.DataType = "";


            string units = ts.Units;
            string type = ts.DataType;

            dataUnits.TryGetValue(units, out units);
            dataTypes.TryGetValue(type, out type);

            item.Units = units;
            item.DataType = type;
          }
          else
          {
            item.Units = ts.Units;
            item.DataType = ts.DataType;
          }
        }
      }
      if (item.RecordType == RecordType.PairedData)
      {
        var pd = GetPairedData(item.FullPath);

        if (pd != null)
        {
          if (pd.UnitsDependent == null)
            pd.UnitsDependent = "";
          if (pd.TypeDependent == null)
            pd.TypeDependent = "";

          if (paths != null && paths._isInterned)
          {
            if (dataUnits == null || dataTypes == null)
              throw new ArgumentException("The paths is interned, but the dataUnits or the dataType hash set is null.  Neither can be null.");

            if (dataUnits == null || dataTypes == null)
              throw new ArgumentException("The paths is interned, but the dataUnits or the dataType hash set is null.  Neither can be null.");

            if (!dataUnits.Contains(pd.UnitsDependent))
              dataUnits.Add(pd.UnitsDependent);
            if (!dataTypes.Contains(pd.TypeDependent))
              dataTypes.Add(pd.TypeDependent);

            string units = pd.UnitsDependent;
            string type = pd.TypeDependent;

            dataUnits.TryGetValue(units, out units);
            dataTypes.TryGetValue(type, out type);

            item.Units = units;
            item.DataType = type;
          }
          else
          {
            item.Units = pd.UnitsDependent;
            item.DataType = pd.TypeDependent;
          }
        }
      }
      if (item.RecordType == RecordType.Grid)
      {
        var grid = GetGrid(item, false);

        if (grid != null)
        {
          if (paths != null && paths._isInterned)
          {
            if (dataUnits == null || dataTypes == null)
              throw new ArgumentException("The paths is interned, but the dataUnits or the dataType hash set is null.  Neither can be null.");

            if (dataUnits == null || dataTypes == null)
              throw new ArgumentException("The paths is interned, but the dataUnits or the dataType hash set is null.  Neither can be null.");

            if (!dataUnits.Contains(grid.DataUnits))
              dataUnits.Add(grid.DataUnits);
            if (!dataTypes.Contains(grid.DataType.ToString()))
              dataTypes.Add(grid.DataType.ToString());

            string units = grid.DataUnits;
            string type = grid.DataType.ToString();

            dataUnits.TryGetValue(units, out units);
            dataTypes.TryGetValue(type, out type);

            item.Units = units;
            item.DataType = type;
          }
          else
          {
            item.Units = grid.DataUnits;
            item.DataType = grid.DataType.ToString();
          }
        }
      }

    }



    /// <summary>
    /// Lookup record type for each path,
    /// required for V6 DSS files
    /// </summary>
    /// <param name="condensed"></param>
    private void SetRecordType(DssPathCollection condensed)
    {
      HashSet<RecordType> uRecordTypeNames = new HashSet<RecordType>();
      foreach (var path in condensed)
      {
        RecordType rt = GetRecordType(path);
        uRecordTypeNames.Add(rt);

        path.RecordType = rt;
      }

      List<RecordType> recordTypeNames = uRecordTypeNames.ToList();
      recordTypeNames.Sort();

      condensed.SetUniqueRecordTypeNames(recordTypeNames);
      condensed.HasRecordTypes = true;
    }


    /// <summary>
    /// Returns true if the value is valid
    /// and not flagged missing or undefined.
    /// </summary>
    /// <param name="value"></param>
    /// <returns></returns>
    public static bool IsValid(double value)
    {
      return value != UNDEFINED_DOUBLE && value != UNDEFINED_DOUBLE_2
        && !IsMissing(value);
    }

    /// <summary>
    /// Returns true if the value represents 'missing'
    /// </summary>
    /// <param name="value"></param>
    /// <returns></returns>
    public static bool IsMissing(double value)
    {
      return value == -901.0
      || value == -902.0;
    }

    public RecordType GetRecordType(DssPath path)
    {
      //Try to get the record type the fast way, which is just assuming the path exists.
      string pathName = path.FullPath;
      if (path is DssPathCondensed)
      {
        pathName = (path as DssPathCondensed).GetComprisingDSSPaths()[0].FullPath;
      }

      int rtInt = DSS.ZDataType(ref ifltab, pathName);

      if (rtInt == -1)
      {
        //Try again, but slower version.

        var catalog = GetCatalog();
        var cmp = new DssPath.DatelessComparer();
        pathName = catalog.Where(p => cmp.Equals(p, path)).First().PathWithoutRange;
        rtInt = DSS.ZDataType(ref ifltab, pathName);
      }

      return RecordTypeFromInt(rtInt);
    }

    /// <summary>
    /// Will tell you if a squeeze for this DSS file is needed
    /// </summary>
    /// <returns>True if squeeze needed, false if squeeze not needed</returns>
    public bool SqueezeNeeded()
    {
      int status = DSS.zSqueezeNeeded(ref ifltab);
      if (status < 0)
        throw new Exception("zSqueezeNeeded reported a failure");
      else if (status == 0)
        return false;
      else
        return true;
    }

    private TimeSeries GetTimeSeries(DssPath dssPath, TimeWindow.ConsecutiveValueCompression compression, DateTime startDateTime = default(DateTime), DateTime endDateTime = default(DateTime), bool PreReadCatalog = true)
    {
      DssPath p = dssPath;
      if (dssPath is DssPathCondensed)
        p = new DssPath(((DssPathCondensed)dssPath).GetPath(0).PathWithoutDate);
      var blockTimeSeries = (CreateTSWrapper(p, startDateTime, endDateTime));
      
      int retrieveFlag = 0;
      int retrieveDoublesFlag = 2; // get doubles
      int boolRetrieveQualityNotes = 1;

      TimeSeries rVal = null;
      TimeSeries innerTimeSeries = new TimeSeries();

      if (PreReadCatalog)
      {
        if (!PathExists(dssPath))
        {
          throw new Exception("Error: Path does not exist '" + dssPath.FullPath + "'");
        }
        RecordType rt = GetRecordType(dssPath);

        if (rt != RecordType.RegularTimeSeries && rt != RecordType.IrregularTimeSeries)
        {
          throw new Exception("Error reading path " + dssPath.FullPath + ".  This path is not a time series");
        } 
      }

      int status = DSS.ZTsRetrieve(ref ifltab, ref blockTimeSeries, retrieveFlag, retrieveDoublesFlag, boolRetrieveQualityNotes);

      // if path is valid, and is time series, (status -1 proably means no data in the time window)

      if (status == 0 )
      {
        SetTimeSeriesInfo(blockTimeSeries, innerTimeSeries);

        if (compression != TimeWindow.ConsecutiveValueCompression.None)
          innerTimeSeries = innerTimeSeries.Compress(compression);

        rVal = innerTimeSeries;
      }
      
      if (rVal == null)
        return GetEmptyTimeSeries(dssPath);

      if (dssPath.IsRegular) // only applies to regular interval
        rVal.Trim();

      return rVal;
      
    }

    private void SetTimeSeriesInfo(ZStructTimeSeriesWrapper fromTimeSeries, TimeSeries ToTimeSeries) 
    {
      ToTimeSeries.Path = new DssPath(fromTimeSeries.Pathname);
      ToTimeSeries.Units = fromTimeSeries.Units;
      ToTimeSeries.Times = GetTsTimes(fromTimeSeries);
      ToTimeSeries.Values = GetTsValues(fromTimeSeries);
      ToTimeSeries.Qualities = fromTimeSeries.Quality;
      ToTimeSeries.DataType = fromTimeSeries.Type;
      ToTimeSeries.ProgramName = fromTimeSeries.ProgramName;
      var locationInfo = new LocationInformation(fromTimeSeries.locationStruct);
      ToTimeSeries.LocationInformation = locationInfo;
    }

    /// <summary>
    /// Gets a DSSTimeSeries from the DSS file.  Can be either irregular or regular time series data.  
    /// data is returned as doubles
    /// If there is a problem, values and times will be null.
    /// </summary>
    ///  <param name="dssPath"></param>
    ///  <param name="startDateTime">starting DateTime (optional overrides path D part)</param>
    /// <param name="endDateTime">ending DateTime (optional overrides path D part)</param>
    /// <param name="behavior"></param>
    /// <returns></returns>
    public TimeSeries GetTimeSeries(DssPath dssPath, DateTime startDateTime = default(DateTime), 
      DateTime endDateTime = default(DateTime), TimeWindow.TimeWindowBehavior behavior = TimeWindow.TimeWindowBehavior.StrictlyInclusive,
      TimeWindow.ConsecutiveValueCompression compression = TimeWindow.ConsecutiveValueCompression.None)
    {
      if (behavior == TimeWindow.TimeWindowBehavior.StrictlyInclusive)
        return GetTimeSeries(dssPath, compression, startDateTime, endDateTime);
      else if (behavior == TimeWindow.TimeWindowBehavior.Cover)
      {
        if (startDateTime == default(DateTime) && endDateTime == default(DateTime))
          return GetTimeSeries(dssPath, compression, startDateTime, endDateTime);
        if (dssPath.IsRegular) // time series is regular
        {
          DateTime t1 = startDateTime.AddSeconds(-TimeWindow.SecondsInInterval(dssPath));
          DateTime t2 = endDateTime.AddSeconds(TimeWindow.SecondsInInterval(dssPath));
          var ts = GetTimeSeries(dssPath, compression, t1, t2);
          ts = TimeWindow.Trim(ts, startDateTime, endDateTime);
          return ts;
        }
        else // time series is irregular
        {
          var ts = GetTimeSeries(dssPath, compression, startDateTime, endDateTime);
          return ts;
        }
      }

      return null;
    }

    public static List<ZStructTimeSeriesWrapper> CreateTSWrapperList(DssPath dssPath, DateTime startDateTime, DateTime endDateTime)
    {
      List<ZStructTimeSeriesWrapper> listTss = new List<ZStructTimeSeriesWrapper>();

      //TODO: When we figure out TODO in CreateTSWrapper, potentially change && to ||
      if (dssPath is DssPathCondensed)
      {//TODO  find contiguous blocks, minimize number of calls.
        var dssPathCon = dssPath as DssPathCondensed;
        for (int i = 0; i < dssPathCon.ComprisedDParts.Count; i++)
        {
          var comprisedPath = dssPathCon.GetPath(i);
          listTss.Add(CreateTSWrapper(comprisedPath));
        }
      }
      else
        listTss.Add(CreateTSWrapper(dssPath, startDateTime, endDateTime));
      return listTss;
    }

    private static ZStructTimeSeriesWrapper CreateTSWrapper(DssPath path, DateTime startDateTime = default(DateTime), DateTime endDateTime = default(DateTime))
    {
      //TODO: Find out how DSS deals with null start and valid end, OR valid start null end.
      //If it handles it like we expect then this method needs to change to accomdate that
      bool datesProvided = startDateTime != default(DateTime) && endDateTime != default(DateTime);
      if (datesProvided)
      {
        string startDate, startTime, endDate, endTime;
        Time.DateTimeToHecDateTime(startDateTime, out startDate, out startTime);
        Time.DateTimeToHecDateTime(endDateTime, out endDate, out endTime);
        return DSS.ZStructTsNewTimes(path.FullPath, startDate, startTime, endDate, endTime);
      }
      else
      {
        return DSS.ZStructTsNew(path.FullPath);
      }

    }

    private static double[] GetTsValues(ZStructTimeSeriesWrapper comprisedTimeSeries)
    {
      if (comprisedTimeSeries.DoubleValues == null)
        throw new NullReferenceException("Time Series double values was null.  Something didn't work right in DSS.");
      double[] values = comprisedTimeSeries.DoubleValues;

      return values;
    }

    private static DateTime[] GetTsTimes(ZStructTimeSeriesWrapper ts)
    {
      if (ts.Times == null)
        throw new NullReferenceException("Time Series Times array was null.  Something didn't work right in DSS.");

      int[] timesJulian = ts.Times;
      DateTime[] times = new DateTime[timesJulian.Length];

      double divisor = (60d * 60d * 24d) / ts.TimeGranularitySeconds;

      // When ts.TimeGranularitySeconds is 60 (e.g. timesJulian is in minutes), this is roughly correct
      // DateTime.FromOADate((double)timesJulian[0] / (60 * 24))
      for (int j = 0; j < times.Length; j++)
      {
        // There appears to be an off-by-1-day error common to julian dates - DEC 1899 vs JAN 1900
        times[j] = DateTime.FromOADate((timesJulian[j] / divisor) + ts.JulianBaseDate + 1);
      }
      return times;
    }

    private static DateTime DateTimeFromJulian_DSSLibrary(int julian, int julianBaseDate = 0, int timeGranularitySeconds = 60)
    {
      // This implementation works, but uses string manipulation and is REALLLY slow
      string d = "".PadRight(20);
      string h = "".PadRight(20);
      DSS.GetDateAndTime(julian, timeGranularitySeconds, julianBaseDate,
          ref d, d.Length, ref h, h.Length);
      return Time.ConvertFromHecDateTime(d, h);
    }


    /// <summary>
    /// GetEmptyTimeSeries returns a time series without the data
    /// used to lookup the units, and data type of a series.
    /// </summary>
    /// <param name="path"></param>
    /// <returns></returns>
    public TimeSeries GetEmptyTimeSeries(DssPath path)
    {
      ZStructTimeSeriesWrapper tss;

      if (path is DssPathCondensed)
      {
        var parts = ((DssPathCondensed)path).GetComprisingDSSPaths();
        path = parts[0];
      }

      tss = DSS.ZStructTsNew(path.FullPath);

      int status = DSS.ZTsRetrieveEmpty(ref ifltab, ref tss);

      var rval = new TimeSeries();
      if (status != 0)
      {
        return rval;
      }
      rval.Path = path;
      rval.Units = tss.Units;
      rval.DataType = tss.Type;
      var locationInfo = new LocationInformation(tss.locationStruct);
      rval.LocationInformation = locationInfo;
      return rval;
    }

    /// <summary>
    /// Gets the double values and ordinates from the paired data at the given path name. If there is a problem, ordinates and values will be null.
    /// </summary>
    /// <param name="pathname">pathname for the paired data</param>
    /// <param name="ordinates">ordinates to return from function</param>
    /// <param name="values">values to return from function</param>
    /// <param name="dataType">An array of size 2, the first value is the type of the independent, the second value is the type of the dependent.</param>
    /// <param name="dataUnits">An array of size 2, the first value is the units of the independent, the second value is the units of the dependent.</param>
    public PairedData GetPairedData(string pathname)
    {
      ZStructPairedDataWrapper pds = DSS.ZStructPdNew(pathname);
      int status = DSS.ZpdRetrieve(ref ifltab, ref pds, 2);
      if (status != 0)
      {
        return null;
      }

      var rval = new PairedData();

      rval.Path = new DssPath(pathname);
      rval.Ordinates = pds.DoubleOrdinates;

      if (pds.FloatValues == null) // doubles
      {
        int k = 0;
        rval.Values = new List<double[]>();
        for (int col = 0; col < pds.NumberCurves; col++)
        {
          var a = new double[pds.NumberOrdinates];
          for (int row = 0; row < pds.NumberOrdinates; row++)
          {
            a[row] = pds.DoubleValues[k++];
          }
          rval.Values.Add(a);
        }
      }
      else if (pds.DoubleValues == null) // convert floats to doubles
      {
        int k = 0;
        rval.Values = new List<double[]>();
        for (int col = 0; col < pds.NumberCurves; col++)
        {
          var a = new double[pds.NumberOrdinates];
          for (int row = 0; row < pds.NumberOrdinates; row++)
          {
            a[row] = pds.FloatValues[k++];
          }
          rval.Values.Add(a);
        }
      }

      if (pds.Labels != null)
      {
        int i = 0;
        int j = 0;
        while (j < pds.Labels.Length) // get labels
        {
          string label = "";
          while (pds.Labels[j] != 0 && j < pds.Labels.Length - 1)
          {
            char c = (char)pds.Labels[j];
            label += c.ToString();
            j++;
          }
          rval.Labels.Add(label);
          j++;
          i++;
        }
      }

      rval.TypeDependent = pds.TypeDependent;
      rval.TypeIndependent = pds.TypeIndependent;
      rval.UnitsDependent = pds.UnitsDependent;
      rval.UnitsIndependent = pds.UnitsIndependent;
      var loc = new LocationInformation(pds.LocationStruct);
      rval.LocationInformation = loc;

      return rval;
    }

    public TimeSeriesProfile GetTimeSeriesProfile(string pathname)
    {
      return GetTimeSeriesProfile(new DssPath(pathname));
    }
    public TimeSeriesProfile GetTimeSeriesProfile(DssPath pathname)
    {
      var rval = new TimeSeriesProfile();
      ZStructTimeSeriesWrapper tss = DSS.ZStructTsNew(pathname.PathWithoutRange);

      int retrieveDoublesFlag = 2; // get doubles
      int retrieveFlagTrim = -1;
      int boolRetrieveQualityNotes = 0;
      int status = DSS.ZTsRetrieve(ref ifltab, ref tss, retrieveFlagTrim, retrieveDoublesFlag, boolRetrieveQualityNotes);
      if (status != 0)
      {
        throw new Exception("Error reading path " + pathname + " in GetTimeSeriesProfile(" + pathname + ")");
      }

      rval.ColumnValues = tss.DoubleProfileDepths;
      rval.Times = GetTsTimes(tss);
      rval.Values = DoubleArrayToMatrix(tss.DoubleProfileValues, rval.ColumnValues.Length, tss.NumberValues);
      return rval;
    }

    private static double[,] DoubleArrayToMatrix(double[] values, int numCols, int numRows)
    {
      double[,] data = new double[numRows, numCols];
      int counter = 0;
      for (int i = 0; i < data.GetLength(0); i++)
      {
        for (int j = 0; j < numCols; j++)
        {
          data[i, j] = values[counter++];
        }
      }

      return data;
    }



    public bool PathExists(string pathname, DateTime startDateTime = default(DateTime), DateTime endDateTime = default(DateTime))
    {
      return PathExists(new DssPath(pathname), startDateTime, endDateTime);
    }
    /// <summary>
    /// Returns true if a path exists.
    /// Ignore D part when startDateTime and endDateTime are provided
    /// if the optional, date range  arguments, are not omitted exact mathching is performed.
    /// </summary>
    /// <param name="path"></param>
    /// <param name="startDateTime"></param>
    /// <param name="endDateTime"></param>
    /// <returns></returns>
    public bool PathExists(DssPath path, DateTime startDateTime = default(DateTime), DateTime endDateTime = default(DateTime))
    {

      bool datesProvided = (startDateTime != default(DateTime) && endDateTime != default(DateTime));

      int count = 0;

      var catalog = GetCatalog();

      var cmp = new DssPath.DatelessComparer();
      if (!datesProvided)
      {
        count = catalog.Count(p => cmp.Equals(p, path));
      }
      else
      {
        var x = catalog.Count(p => cmp.Equals(p, path));

        count = catalog.Count(p => cmp.Equals(p, path)
              && (p is DssPathCondensed
                       && Time.ConvertFromHecDateTime((p as DssPathCondensed).DPartStart) >= startDateTime
                       && Time.ConvertFromHecDateTime((p as DssPathCondensed).DPartEnd) <= endDateTime));
      }

      return count > 0;

    }


    /// <summary>
    /// Checks if the path exists in the DSS file.
    /// exact match required
    /// </summary>
    /// <param name="path"></param>
    /// <returns>True if the path exists, false otherwise.</returns>
    public bool ExactPathExists(DssPath path)
    {
      int status = DSS.ZCheck(ifltab, path.FullPath);
      if (status != 0)
        return false;
      else
        return true;
    }

    /// <summary>
    /// Gives you the value used for flagging missing values in time series data.  Compatible with DSS 6 and 7
    /// </summary>
    /// <returns>The missing flag value</returns>
    public float CheckMissingFlag()
    {
      return DSS.ZMissingFlag();
    }

    /// <summary>
    /// Will tell you the file version of the currently opened DSS file.
    /// </summary>
    /// <returns>6 or 7 depending on the version</returns>
    public int GetDSSFileVersion()
    {
      return DSS.ZGetVersion(ref ifltab);
    }



    /// <summary>
    /// Retrieves location Info data
    /// </summary>
    /// <param name="pathname">pathname to the time series in DSS.</param>
    /// <returns>If successful, the location data in the pathname.  Otherwise returns empty LocationData
    public LocationInformation GetLocationInfo(string pathname)
    {
      ZStructLocationWrapper w = DSS.ZLocationRetrieve(ref ifltab, pathname);
      var L = new LocationInformation(w);
      return L;
    }


    /// <summary>
    /// Gives you a grid object with a specified path name in the DSS file
    /// </summary>
    /// <param name="pathname">pathname to the grid</param>
    /// <param name="retrieveData">Whether to retrieve data or not</param>
    /// <returns>Returns an albers grid if the grid is albers, a specified grid if its a specified grid, or a generic grid otherwise.  Returns null if there is a problem.</returns>
    public Grid GetGrid(string pathname, bool retrieveData)
    {
      ZStructSpatialGridWrapper grid = DSS.ZStructSpatialGridNew(pathname);
      int status = DSS.ZSpatialGridRetrieve(ref ifltab, ref grid, retrieveData);
      if (status != 0)
        return null;

      return new Grid(grid, retrieveData);
    }

    /// <summary>
    /// Gives you a grid object with a specified path name in the DSS file
    /// </summary>
    /// <param name="path">pathname to the grid</param>
    /// <param name="retrieveData">Whether to retrieve data or not</param>
    /// <returns>Returns an albers grid if the grid is albers, a specified grid if its a specified grid, or a generic grid otherwise.  Returns null if there is a problem.</returns>
    public Grid GetGrid(DssPath path, bool retrieveData)
    {
      return GetGrid(path.FullPath, retrieveData);
    }

    public void Dispose()
    {
      ActiveReaders.Remove(this);
      DSS.ZClose(ifltab);
      _iflTabGC.Free();
    }

    public enum LevelID
    {
      /// <summary>
      /// No messages, including errors (not guaranteed).  Highly discourage
      /// </summary>
      MESS_LEVEL_NONE = 0,
      /// <summary>
      /// Critical (Error) Messages.  Use discouraged.
      /// </summary>
      MESS_LEVEL_CRITICAL = 1,
      /// <summary>
      /// Minimal (terse) output:  zopen, zclose, critical errors.
      /// </summary>
      MESS_LEVEL_TERSE = 2,
      /// <summary>
      /// General Log Messages.  Default.
      /// </summary>
      MESS_LEVEL_GENERAL = 3,
      /// <summary>
      /// Diagnostic User Messages (e.g., input parameters)
      /// </summary>
      MESS_LEVEL_USER_DIAG = 4,
      /// <summary>
      /// Diagnostic Internal Messages level 1 (debug).   Not recommended for users
      /// </summary>
      MESS_LEVEL_INTERNAL_DIAG_1 = 5,
      /// <summary>
      /// Diagnostic Internal Messages level 2 (full debug)
      /// </summary>
      MESS_LEVEL_INTERNAL_DIAG_2 = 6,
    }

    public enum MethodID
    {
      /// <summary>
      /// All methods both in DSS version 6 and 7
      /// </summary>
      MESS_METHOD_GLOBAL_ID = 0,
      /// <summary>
      /// All methods in DSS version 7 (includes those below)
      /// </summary>
      MESS_METHOD_GENERAL_ID = 1,
      /// <summary>
      /// Low-level read I/O
      /// </summary>
      MESS_METHOD_GET_ID = 2,
      /// <summary>
      /// Low-level write I/O
      /// </summary>
      MESS_METHOD_PUT_ID = 3,
      /// <summary>
      /// Read methods, except time series
      /// </summary>
      MESS_METHOD_READ_ID = 4,
      /// <summary>
      /// Write methods, except time series
      /// </summary>
      MESS_METHOD_WRITE_ID = 5,
      /// <summary>
      /// Operations for the file header
      /// </summary>
      MESS_METHOD_PERM_ID = 6,
      /// <summary>
      /// Opening and creating a DSS file
      /// </summary>
      MESS_METHOD_OPEN_ID = 7,
      /// <summary>
      /// Checking for records
      /// </summary>
      MESS_METHOD_CHECK_ID = 8,
      /// <summary>
      /// Locking and unlocking methods
      /// </summary>
      MESS_METHOD_LOCKING_ID = 9,
      /// <summary>
      /// Time series read operations
      /// </summary>
      MESS_METHOD_TS_READ_ID = 10,
      /// <summary>
      /// Time series write operations
      /// </summary>
      MESS_METHOD_TS_WRITE_ID = 11,
      /// <summary>
      /// Record alias methods
      /// </summary>
      MESS_METHOD_ALIAS_ID = 12,
      /// <summary>
      /// Record copying functions
      /// </summary>
      MESS_METHOD_COPY_ID = 13,
      /// <summary>
      /// General utility functions (rename, delete, etc.)
      /// </summary>
      MESS_METHOD_UTILITY_ID = 14,
      /// <summary>
      /// Cataloging
      /// </summary>
      MESS_METHOD_CATALOG_ID = 15,
      /// <summary>
      /// Checking file integrity
      /// </summary>
      MESS_METHOD_FILE_CHECK_ID = 16,
      /// <summary>
      /// Java Native Interface
      /// </summary>
      MESS_METHOD_JNI_ID = 17
    }
  }


}

