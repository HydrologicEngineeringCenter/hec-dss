using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Hec.Dss
{
  public class DssPathCondensed : DssPath
  {
    static DssPathCondensed() => Assembly.Initialize();

    private List<string> _comprisedDParts;
    internal readonly string DPartStart;
    internal readonly string DPartEnd;

    public new static DssPathCondensed NotFound = new DssPathCondensed(DssPath.NotFound.FullPath, new List<string>());
    

    public List<string> ComprisedDParts
    {
      get { return _comprisedDParts; }
    }

    /// <summary>
    /// returns date portion (D) of path in a sortable format ToString("yyyyMMdd");
    /// </summary>
    public override string SortableDPart
    {
      get
      {
        if (!Time.TryConvertFromHecDateTime(DPartStart, out DateTime date))
          return Dpart;
        else
          return date.ToString("yyyyMMdd");
      }
    }

    public DssPathCondensed(string Path, List<string> comprisedDParts) : base(Path)
    {
      _comprisedDParts = comprisedDParts;
      if (_comprisedDParts.Count > 0)
      {
        DPartStart = _comprisedDParts[0];
        DPartEnd = _comprisedDParts[_comprisedDParts.Count - 1];
        if (_comprisedDParts.Count > 1)
          Dpart = DPartStart + "-" + DPartEnd;
        else
          Dpart = DPartStart;
      }
    }

    public DssPathCondensed(string A, string B, string C, List<string> comprisedDParts, string E, string F, RecordType recordTypeName = RecordType.Unknown, 
      string dataType = "", string dataUnits = "", double xOrdinate = 0, double yOrdinate = 0, double zOrdinate = 0)
      : base(A, B, C, default(DateTime), E, F, recordTypeName, dataType, dataUnits, xOrdinate, yOrdinate, zOrdinate)
    {
      _comprisedDParts = comprisedDParts;
      DPartStart = _comprisedDParts[0];
      DPartEnd = _comprisedDParts[_comprisedDParts.Count - 1];
      if (_comprisedDParts.Count > 1)
        Dpart = DPartStart + "-" + DPartEnd;
      else
        Dpart = DPartStart;
    }

    /// <summary>
    /// Forms a new path object for each comprising Dpart, copying all parts from the condensed path except the D part
    /// </summary>
    /// <returns></returns>
    public List<DssPath> GetComprisingDSSPaths()
    {
      List<DssPath> toReturn = new List<DssPath>();
      for (int i = 0; i < _comprisedDParts.Count; i++)
      {
        DssPath p = new DssPath(Apart, Bpart, Cpart, _comprisedDParts[i], Epart, Fpart);
        p.RecordType = RecordType;
        p.DataType = DataType;
        p.Units = Units;
        p.XOrdinate = XOrdinate;
        p.YOrdinate = YOrdinate;
        p.ZOrdinate = ZOrdinate;
        toReturn.Add(p);
      }

      return toReturn;
    }

    /// <summary>
    /// Forms a new path object for a given index in the comprisedDParts, copying all parts from the condensed path except the D part
    /// </summary>
    public DssPath GetPath(int index)
    {
      DssPath p = new DssPath(Apart, Bpart, Cpart, _comprisedDParts[index], Epart, Fpart);
      p.DataType = DataType;
      p.Units = Units;
      p.XOrdinate = XOrdinate;
      p.YOrdinate = YOrdinate;
      p.ZOrdinate = ZOrdinate;

      return p;
    }

    /// <summary>
    /// Gets the D part as a DateTime Range, using a 2-tuple.  Does not check if valid range before executing.
    /// </summary>
    /// <returns>A tuple with the range.  If only 1 value then the 2nd item will be DateTime default which is DateTime.MinValue</returns>
    public Tuple<DateTime, DateTime> GetDPartAsDateTimeRange()
    {
      string[] splitDPart = Dpart.Split('-');
      if (splitDPart.Length == 2)
      {
        return new Tuple<DateTime, DateTime>(Time.ConvertFromHecDateTime(splitDPart[0]), Time.ConvertFromHecDateTime(splitDPart[1]));
      }
      else if (splitDPart.Length == 1)
      {
        return new Tuple<DateTime, DateTime>(Time.ConvertFromHecDateTime(splitDPart[0]), default(DateTime));
      }
      else
      {
        throw new Exception("Invalid D Part formatting");
      }

    }
  }
}
