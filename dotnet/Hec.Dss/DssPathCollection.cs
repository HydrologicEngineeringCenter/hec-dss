using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;

namespace Hec.Dss
{
  public class DssPathCollection : IList<DssPath>
  {
    static DssPathCollection() => Assembly.Initialize();

    public DataTable ToDataTable()
    {
      var table = new DataTable();
      table.Columns.Add("index", typeof(int));
      table.Columns.Add("A");
      table.Columns.Add("B");
      table.Columns.Add("C");
      table.Columns.Add("D");
      table.Columns.Add("E");
      table.Columns.Add("F");
      table.Columns.Add("RecordType");
      table.Columns.Add("DataType");
      table.Columns.Add("Units");
      table.Columns.Add("XOrdinate");
      table.Columns.Add("YOrdinate");
      table.Columns.Add("ZOrdinate");
      table.PrimaryKey = new DataColumn[] { table.Columns[0] };

      for (int i = 0; i < Paths.Count; i++)
      {
        var p = Paths[i];
        table.Rows.Add(i, p.Apart, p.Bpart, p.Cpart, p.Dpart, p.Epart, p.Fpart,
            p.RecordType.ToString(), p.DataType, p.Units, p.XOrdinate, p.YOrdinate, p.ZOrdinate);
      }
      return table;
    }

    public string Filename { get; private set; }
    public DateTime FileDateTime { get; private set; }

    public IList<DssPath> Paths = new List<DssPath>();

    public bool HasRecordTypes { get; internal set; }

    public int Count => Paths.Count;

    public bool IsReadOnly => throw new NotImplementedException();

    public DssPath this[int index] { get => Paths[index]; set => Paths[index] = value; }

    private IList<string> _aParts;
    private IList<string> _bParts;
    private IList<string> _cParts;
    private IList<string> _dParts;
    private IList<string> _eParts;
    private IList<string> _fParts;

    /// <summary>
    ///  locationInfoDict is a dictionary that maps a tuple of aPart and bPart to a LocationInformation object.
    ///  It is used to quicky determine if 'Location Info' exists for a given aPart and bPart.  
    ///     If it exists it will initially be a 'null' entry , by constructor - (for later retrieval) 
    ///     If it doesn't exist it will not be in the dictionary (most common case)
    /// </summary>
    internal Dictionary<Tuple<string, string>, LocationInformation> locationInfoDict;

    //has to be internal since it's possible it won't be constructed correctly on object construction.  It relies on DSSReader to construct it correctly in that case.
    internal IList<RecordType> _recordTypeNames;
    internal IList<string> _dataUnits;
    internal IList<string> _dataTypes;
    internal IList<double> _xs;
    internal IList<double> _ys;
    internal IList<double> _zs;

    internal bool _isInterned = false;

    /// <summary>
    /// Constructor for DSSPathCollection which has string interning.
    /// </summary>
    /// <param name="strPaths">raw list of paths (unsorted)</param>
    /// <param name="
    /// ">when true builds condenced paths </param>
    internal DssPathCollection(string fileName, string[] strPaths, RecordType[] recordTypes, bool condensed)
    {

      Filename = fileName;
      if (System.IO.File.Exists(Filename)) FileDateTime = System.IO.File.GetLastWriteTime(Filename);

      var unsorted = new List<DssPath>();
      Paths = new List<DssPath>();
      //  hashsets used to generate unique parts
      HashSet<string> uAParts = new HashSet<string>();
      HashSet<string> uBParts = new HashSet<string>();
      HashSet<string> uCParts = new HashSet<string>();
      HashSet<string> uDParts = new HashSet<string>();
      HashSet<string> uEParts = new HashSet<string>();
      HashSet<string> uFParts = new HashSet<string>();
      HashSet<RecordType> uRecordTypeNames = new HashSet<RecordType>();
      locationInfoDict = new Dictionary<Tuple<string, string>, LocationInformation>();

      if (recordTypes == null)
        HasRecordTypes = false;
      else
        HasRecordTypes = true;

      for (int i = 0; i < strPaths.Length; i++)
      {
        string[] splitPath = strPaths[i].Split('/');
        string aPart = splitPath[1];
        string bPart = splitPath[2];
        string cPart = splitPath[3];
        string dPart = splitPath[4];
        string ePart = splitPath[5];
        string fPart = splitPath[6];

        if (!uAParts.Contains(aPart))
          uAParts.Add(aPart);
        if (!uBParts.Contains(bPart))
          uBParts.Add(bPart);
        if (!uCParts.Contains(cPart))
          uCParts.Add(cPart);
        if (!uDParts.Contains(dPart))
          uDParts.Add(dPart);
        if (!uEParts.Contains(ePart))
          uEParts.Add(ePart);
        if (!uFParts.Contains(fPart))
          uFParts.Add(fPart);

        
        if (cPart == "Location Info")
        {
          var abKey = new Tuple<string, string>(aPart, bPart);
          if (!locationInfoDict.ContainsKey(abKey))     {
              locationInfoDict[abKey] = null;
          }
        }


        uAParts.TryGetValue(aPart, out aPart);
        uBParts.TryGetValue(bPart, out bPart);
        uCParts.TryGetValue(cPart, out cPart);
        uDParts.TryGetValue(dPart, out dPart);
        uEParts.TryGetValue(ePart, out ePart);
        uFParts.TryGetValue(fPart, out fPart);
        var p = new DssPath(aPart, bPart, cPart, dPart, ePart, fPart);

        if (!HasRecordTypes || i >= recordTypes.Length)
          p.RecordType = RecordType.Unknown;
        else
        {
          if (!uRecordTypeNames.Contains(recordTypes[i]))
          {
            uRecordTypeNames.Add(recordTypes[i]);
          }
          uRecordTypeNames.TryGetValue(recordTypes[i], out RecordType recordType);
          p.RecordType = recordType;
        }
        unsorted.Add(p);
      }
      //  
      //  convert the unsorted hashsets to lists and sort them 
      _aParts = uAParts.ToList();
      (_aParts as List<string>).Sort();
      _bParts = uBParts.ToList();
      (_bParts as List<string>).Sort();
      _cParts = uCParts.ToList();
      (_cParts as List<string>).Sort();
      _dParts = uDParts.ToList();
      (_dParts as List<string>).Sort();
      _eParts = uEParts.ToList();
      (_eParts as List<string>).Sort();
      _fParts = uFParts.ToList();
      (_fParts as List<string>).Sort();
      _recordTypeNames = uRecordTypeNames.ToList();
      (_recordTypeNames as List<RecordType>).Sort();

      UnCondensedPaths = unsorted;
      CondensedPaths = new List<DssPathCondensed>();
      if (!condensed)
      {
        Paths = UnCondensedPaths;
        GetCondensedPaths(unsorted);
        return;
      }
      GetCondensedPaths(unsorted);

      // sort each group by date then sort the groups
      // (sorting before grouping is 15x - 20x slower)
      (CondensedPaths as List<DssPathCondensed>).Sort();
      (Paths as List<DssPath>).Sort();
      _isInterned = true;
    }

    private void GetCondensedPaths(List<DssPath> unsorted)
    {
      List<DssPath> otherPaths = new List<DssPath>();
      List<DssPath> timeSeriesPaths = new List<DssPath>();

      foreach (var path in unsorted)
      {
        if( !path.IsTimeSeries())
        {
          otherPaths.Add(path);
          // Add non time series path to paths list.
          Paths.Add(path);
        }
        else
          timeSeriesPaths.Add(path);
      }

      // use linq to group by pathnames without D part
      var groups = timeSeriesPaths.GroupBy(dssPath => dssPath, new DssPath.DatelessComparer()).Select(grp => grp.OrderBy(dssp => dssp.DPartAsDateTime).ToList()).ToList();
      foreach (var group in groups)
      {
        if (group.Count == 1)
        {
          CondensedPaths.Add(new DssPathCondensed(group[0].PathWithoutDate, new List<string>() { group[0].Dpart } ));
          Paths.Add(group[0]);
        }
        else
        {
          var p = new DssPathCondensed(group[0].Apart, group[0].Bpart, group[0].Cpart, group.Select(path => path.Dpart).ToList(), group[0].Epart, group[0].Fpart, group[0].RecordType);
          CondensedPaths.Add(p);
          Paths.Add(p);
        }
      }

      // Add non time series paths to condensed paths list.
      foreach (var path in otherPaths)
      {
        CondensedPaths.Add(new DssPathCondensed(path.Apart, path.Bpart, path.Cpart, new List<string> { path.Dpart }, path.Epart, path.Fpart, path.RecordType));
      }
    }

    /// <summary>
    /// NOTE: Assuming every list passed is sorted.
    /// </summary>
    /// <param name="fileName"></param>
    /// <param name="paths"></param>
    /// <param name="uniqueAs"></param>
    /// <param name="uniqueBs"></param>
    /// <param name="uniqueCs"></param>
    /// <param name="uniqueDs"></param>
    /// <param name="uniqueEs"></param>
    /// <param name="uniqueFs"></param>
    /// <param name="uniqueRecordTypeNames"></param>
    /// <param name="uniqueDataTypes"></param>
    /// <param name="uniqueDataUnits"></param>
    /// <param name="uniqueXs"></param>
    /// <param name="uniqueYs"></param>
    /// <param name="uniqueZs"></param>
    public DssPathCollection(string fileName, IList<DssPath> paths, IList<string> uniqueAs = null, IList<string> uniqueBs = null, IList<string> uniqueCs = null,
      IList<string> uniqueDs = null, IList<string> uniqueEs = null, IList<string> uniqueFs = null, IList<RecordType> uniqueRecordTypeNames = null, IList<string> uniqueDataTypes = null, IList<string> uniqueDataUnits = null,
      IList<double> uniqueXs = null, IList<double> uniqueYs = null, IList<double> uniqueZs = null)
    {
      Filename = fileName;
      if (System.IO.File.Exists(Filename)) FileDateTime = System.IO.File.GetLastWriteTime(Filename);

      if (uniqueAs != null && uniqueBs != null && uniqueCs != null && uniqueDs != null && uniqueEs != null && uniqueFs != null)
        _isInterned = true;

      _aParts = uniqueAs;
      _bParts = uniqueBs;
      _cParts = uniqueCs;
      _dParts = uniqueDs;
      _eParts = uniqueEs;
      _fParts = uniqueFs;
      _recordTypeNames = uniqueRecordTypeNames;
      _dataUnits = uniqueDataUnits;
      _dataTypes = uniqueDataTypes;
      _xs = uniqueXs;
      _ys = uniqueYs;
      _zs = uniqueZs;

      Paths = paths;
      UnCondensedPaths = new List<DssPath>();
      CondensedPaths = new List<DssPathCondensed>();
    }

    public static void WriteToFile(DssPathCollection paths, string filename)
    {
      using (StreamWriter sw = new StreamWriter(filename))
      {
        sw.WriteLine("index,FullPath,PathWithoutDate");
        for (int i = 0; i < paths.Count; i++)
        {
          var p = paths.Paths[i];
          sw.WriteLine(i + "," + p.FullPath + ", " + p.PathWithoutDate);
        }
      }
    }

    public IList<DssPathCondensed> CondensedPaths { get; private set; }
    public IList<DssPath> UnCondensedPaths { get; private set; }

    public IList<string> GetUniqueAParts()
    {
      if (_aParts == null)
        _aParts = PathAssist.GetUniqueAParts(Paths);

      return _aParts;
    }

    public IList<string> GetUniqueBParts()
    {
      if (_bParts == null)
        _bParts = PathAssist.GetUniqueBParts(Paths);

      return _bParts;
    }

    public IList<string> GetUniqueCParts()
    {
      if (_cParts == null)
        _cParts = PathAssist.GetUniqueCParts(Paths);

      return _cParts;
    }
    public IList<string> GetUniqueDParts()
    {
      if (_dParts == null)
        _dParts = PathAssist.GetUniqueDParts(Paths);

      return _dParts;
    }
    public IList<string> GetUniqueEParts()
    {
      if (_eParts == null)
        _eParts = PathAssist.GetUniqueEParts(Paths);

      return _eParts;
    }
    public IList<string> GetUniqueFParts()
    {
      if (_fParts == null)
        _fParts = PathAssist.GetUniqueFParts(Paths);

      return _fParts;
    }

    public IList<RecordType> GetUniqueRecordTypeNames()
    {
      if (_recordTypeNames == null)
        _recordTypeNames = PathAssist.GetUniqueRecordTypeNames(Paths);

      return _recordTypeNames;
    }

    internal void SetUniqueRecordTypeNames(List<RecordType> uniqueRecordTypes)
    {
      _recordTypeNames = uniqueRecordTypes;
    }

    public IList<string> GetUniqueDataTypes()
    {
      if (_dataTypes == null)
        _dataTypes = PathAssist.GetUniqueDataTypes(Paths);

      return _dataTypes;
    }

    internal void SetUniqueDataTypes(List<string> uniqueDataTypes)
    {
      _dataTypes = uniqueDataTypes;
    }

    public IList<string> GetUniqueDataUnits()
    {
      if (_dataUnits == null)
        _dataUnits = PathAssist.GetUniqueUnits(Paths);

      return _dataUnits;
    }

    internal void SetUniqueDataUnits(List<string> uniqueDataUnits)
    {
      _dataUnits = uniqueDataUnits;
    }

    public IList<double> GetUniqueXs()
    {
      if (_xs == null)
        _xs = PathAssist.GetUniqueXs(Paths);

      return _xs;
    }

    internal void SetUniqueXs(List<double> uniqueXs)
    {
      _xs = uniqueXs;
    }

    public IList<double> GetUniqueYs()
    {
      if (_ys == null)
        _ys = PathAssist.GetUniqueYs(Paths);

      return _ys;
    }

    internal void SetUniqueYs(List<double> uniqueYs)
    {
      _ys = uniqueYs;
    }

    public IList<double> GetUniqueZs()
    {
      if (_zs == null)
        _zs = PathAssist.GetUniqueZs(Paths);

      return _zs;
    }

    internal void SetUniqueZs(List<double> uniqueZs)
    {
      _zs = uniqueZs;
    }



    public List<DssPath> FilterByPart(string aPart = "", string bPart = "", string cPart = "", string dPart = "", string ePart = "", string fPart = "")
    {
      return PathAssist.FilterByPart(Paths, aPart, bPart, cPart, dPart, ePart, fPart);
    }

    /// <summary>
    /// Could take a while if there are many paths, is O(n)
    /// </summary>
    /// <returns></returns>
    public DssPath FindExactPath(string path)
    {
      for (int i = 0; i < Paths.Count; i++)
      {
        if (path == Paths[i].FullPath)
        {
          return Paths[i];
        }
      }
      return DssPath.NotFound;
    }

    public DssPathCondensed GetCondensedPath(DssPath path)
    {
      var paths = PathAssist.FilterByPart(this, path.Apart, path.Bpart, path.Cpart, "", path.Epart, path.Fpart);
      for (int i = 0; i < paths.Count; i++)
      {
        //Check if the d part is the same DT or is in the range of DTs.
        string dpart = path.Dpart;
        if (path.Dpart == null)
          dpart = "";
        if (path.IsDPartARange())
          dpart = dpart.Split('-')[0].Trim();

        if (Time.TryConvertFromHecDateTime(dpart, out DateTime dPartAsDT))
        {
          if (paths[i] is DssPathCondensed)
          {
            var condensedP = paths[i] as DssPathCondensed;
            var DTrange = condensedP.GetDPartAsDateTimeRange();

            if (dPartAsDT >= DTrange.Item1 && dPartAsDT <= DTrange.Item2)
              return paths[i] as DssPathCondensed;
          }
          else
          {//not condensed, but we can return it as condensed.
            List<string> comprised = new List<string>();
            comprised.Add(paths[i].Dpart);
            return new DssPathCondensed(paths[i].FullPath, comprised);
          }

        }
      }

      return DssPathCondensed.NotFound;
    }

    public bool TryGetCondensedPath(DssPath path, out DssPathCondensed resultPath)
    {
      resultPath = GetCondensedPath(path);

      if (resultPath == DssPathCondensed.NotFound)
        return false;

      return true;
    }

    public int IndexOf(DssPath item)
    {
      return Paths.IndexOf(item);
    }

    public void Insert(int index, DssPath item)
    {
      Paths.Insert(index, item);
    }

    public void RemoveAt(int index)
    {
      Paths.RemoveAt(index);
    }

    public void Add(DssPath item)
    {
      Paths.Add(item);
    }

    public void Clear()
    {
      Paths.Clear();
    }

    public bool Contains(DssPath item)
    {
      return Paths.Contains(item);
    }

    public void CopyTo(DssPath[] array, int arrayIndex)
    {
      Paths.CopyTo(array, arrayIndex);
    }

    public bool Remove(DssPath item)
    {
      return Paths.Remove(item);
    }

    public IEnumerator<DssPath> GetEnumerator()
    {
      return Paths.GetEnumerator();
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
      return Paths.GetEnumerator();
    }
  }
}
