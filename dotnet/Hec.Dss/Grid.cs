using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Hec.Dss
{
  /// <summary>
  /// DSS Data type (statistic)
  /// </summary>
  public enum DssDataType
  {
    PER_AVER = 0,
    PER_CUM = 1,
    INST_VAL = 2,
    INST_CUM = 3,
    FREQ = 4,
    INVAL = 5
  }

  /// <summary>
  /// DSS Record Type
  /// </summary>
  public enum DssGridType
  {
    UNDEFINED_GRID_TYPE = 400,
    HRAP = 410,
    ALBERS = 420,
    SPECIFIED_GRID_TYPE = 430
  }

  public class Grid
  {
    static Grid() => Assembly.Initialize();

    protected NativeSpatialGridWrapper _grid;

    public string PathName
    {
      get { return _grid.PathName; }
      set { _grid.PathName = value; }
    }

    public DssGridType GridType
    {
      get { return (DssGridType)_grid.GridType; } // readonly. 
    }
    public NativeSpatialGridWrapper DSSObj
    {
      get
      {
        return _grid;
      }
    }


    public string DataUnits
    {
      get
      {
        return _grid.DataUnits;
      }
      set
      {
        _grid.DataUnits = value;
      }
    }

    /// <summary>
    /// DSS data type (statistic type for the data)
    /// PER_AVER = 0, PER_CUM = 1, INST_VAL = 2, INST_CUM = 3, FREQ = 4, INVAL = 5
    /// </summary>
    public DssDataType DataType
    {
      get
      {
        return (DssDataType)_grid.DataType;
      }
      set
      {
        _grid.DataType = (int)value;
      }
    }

    public int LowerLeftCellX
    {
      get
      {
        return _grid.LowerLeftCellX;
      }
      set
      {
        _grid.LowerLeftCellX = value;
      }
    }

    public int LowerLeftCellY
    {
      get
      {
        return _grid.LowerLeftCellY;
      }
      set
      {
        _grid.LowerLeftCellY = value;
      }
    }

    public int NumberOfCellsX
    {
      get
      {
        return _grid.NumberOfCellsX;
      }
      set
      {
        _grid.NumberOfCellsX = value;
      }
    }

    public int NumberOfCellsY
    {
      get
      {
        return _grid.NumberOfCellsY;
      }
      set
      {
        _grid.NumberOfCellsY = value;
      }
    }

    public float CellSize
    {
      get
      {
        return _grid.CellSize;
      }
      set
      {
        _grid.CellSize = value;
      }
    }

    public string SRSName
    {
      get
      {
        return _grid.SRSName;
      }
      set
      {
        _grid.SRSName = value;
      }
    }

    public int SRSDefinitionType
    {
      get
      {
        return _grid.SRSDefinitionType;
      }
      set
      {
        _grid.SRSDefinitionType = value;
      }
    }

    public string SRSDefinition
    {
      get
      {
        return _grid.SRSDefinition;
      }
      set
      {
        _grid.SRSDefinition = value;
      }
    }

    public float XCoordOfGridCellZero
    {
      get
      {
        return _grid.XCoordOfGridCellZero;
      }
      set
      {
        _grid.XCoordOfGridCellZero = value;
      }
    }

    public float YCoordOfGridCellZero
    {
      get
      {
        return _grid.YCoordOfGridCellZero;
      }
      set
      {
        _grid.YCoordOfGridCellZero = value;
      }
    }

    public string Info()
    {
      StringBuilder sb = new StringBuilder();

      sb.AppendLine("******** Printing GRID STRUCT ********");
      //sb.AppendLine("Storage Data Type  :"+ _grid.StorageDataType);
      sb.AppendLine("GridStruct Version :" + _grid.StructVersion);
      sb.AppendLine("Path :" + _grid.PathName);
      sb.AppendLine("GridType :" + _grid.GridType);
      sb.AppendLine("Version : " + _grid.Version);
      sb.AppendLine("Data Units : " + _grid.DataUnits);
      sb.AppendLine("Data Type : " + _grid.DataType);
      sb.AppendLine("Data Source : " + _grid.StorageDataType);
      sb.AppendLine("LowerLeftCellX : " + _grid.LowerLeftCellX);
      sb.AppendLine("LowerLeftCellY : " + _grid.LowerLeftCellY);
      sb.AppendLine("NumberOfCellsX : " + _grid.NumberOfCellsX);
      sb.AppendLine("NumberOfCellsY : " + _grid.NumberOfCellsY);
      sb.AppendLine("CellSize : " + _grid.CellSize.ToString("0.00000"));
      sb.AppendLine("CompressionMethod : " + _grid.CompressionMethod);
      sb.AppendLine("SizeofCompressedElements : " + _grid.SizeOfCompressedElements);

      sb.AppendLine("NumberOfRanges : " + _grid.NumberOfRanges);
      sb.AppendLine("SrsName : " + _grid.SRSName);
      sb.AppendLine("SrsDefinitionType : " + _grid.SRSDefinitionType);
      sb.AppendLine("_srsDefinition : " + _grid.SRSDefinition);
      sb.AppendLine("XCoordOfGridCellZero : " + _grid.XCoordOfGridCellZero.ToString("0.00000"));
      sb.AppendLine("YCoordOfGridCellZero : " + _grid.YCoordOfGridCellZero.ToString("0.00000"));
      sb.AppendLine("NullValue : " + _grid.NullValue.ToString("0.00000"));
      sb.AppendLine("TimeZoneID : " + _grid.TimeZoneID);
      sb.AppendLine("timeZoneRawOffset : " + _grid.TimeZoneRawOffset);
      sb.AppendLine("IsInterval : " + _grid.IsInterval);
      sb.AppendLine("IsTimeStamped : " + _grid.IsTimeStamped);
      sb.AppendLine("Storage Data Type  :" + _grid.StorageDataType);

      if (_grid.StorageDataType == 0) // GRID_FLOAT
      {
        sb.AppendLine("Max Data Value : " + _grid.MaxDataValue.ToString("0.00000"));
        sb.AppendLine("Min Data Value : " + _grid.MinDataValue.ToString("0.00000"));
        sb.AppendLine("Mean Data Value : " + _grid.MeanDataValue.ToString("0.00000"));
        sb.AppendLine("======== Range Limit Table ===========");

        if (_grid.RangeLimitTable != null && _grid.RangeLimitTable.Length > 0)
        {
          sb.AppendLine("           Range        > or =    Incremental Count");
          int size = _grid.NumberOfRanges;
          for (int i = 0; i < size - 1; i++)
          {
            var v = _grid.RangeLimitTable[i];
            var vs = v.ToString("0.00000");
            if (v == _grid.UndefinedValue)
              vs = "undefined".PadRight(16);
            sb.AppendLine(vs + " "
                + _grid.NumberEqualOrExceedingRangeLimit[i].ToString("0.00000"));
          }

          sb.AppendLine("====================================================");
        }
      }
      return sb.ToString();
    }

    public float NullValue
    {
      get
      {
        return _grid.NullValue;
      }
      set
      {
        _grid.NullValue = value;
      }
    }

    public float UndefinedValue
    {
      get { return _grid.UndefinedValue; }
    }

    public string TimeZoneID
    {
      get
      {
        return _grid.TimeZoneID;
      }
      set
      {
        _grid.TimeZoneID = value;
      }
    }

    public int TimeZoneRawOffset
    {
      get
      {
        return _grid.TimeZoneRawOffset;
      }
      set
      {
        _grid.TimeZoneRawOffset = value;
      }
    }

    public bool IsInterval
    {
      get
      {
        return _grid.IsInterval;
      }
      set
      {
        _grid.IsInterval = value;
      }
    }

    public bool IsTimeStamped
    {
      get
      {
        return _grid.IsTimeStamped;
      }
      set
      {
        _grid.IsTimeStamped = value;
      }
    }

    public int NumberOfRanges
    {
      get
      {
        return _grid.NumberOfRanges;
      }
      set
      {
        _grid.NumberOfRanges = value;
      }
    }

    public int StorageDataType
    {
      get
      {
        return _grid.StorageDataType;
      }
      set
      {
        _grid.MinDataValue = value;
      }
    }

    public float MinDataValue
    {
      get
      {
        return _grid.MinDataValue;
      }
      set
      {
        _grid.MinDataValue = value;
      }
    }

    public float MaxDataValue
    {
      get
      {
        return _grid.MaxDataValue;
      }
      set
      {
        _grid.MaxDataValue = value;
      }
    }

    public float MeanDataValue
    {
      get
      {
        return _grid.MeanDataValue;
      }
      set
      {
        _grid.MeanDataValue = value;
      }
    }

    public float[] RangeLimitTable
    {
      get
      {
        return _grid.RangeLimitTable;
      }
      set
      {
        _grid.RangeLimitTable = value;
      }
    }

    public int[] NumberEqualOrExceedingRangeLimit
    {
      get
      {
        return _grid.NumberEqualOrExceedingRangeLimit;
      }
      set
      {
        _grid.NumberEqualOrExceedingRangeLimit = value;
      }
    }


    private float[] _data = null;

    public float[] Data
    {
      get
      {
        return _data;
      }
      set
      {
        _grid.Data = value;
        _data = value;
      }
    }

    /// <summary>
    /// Grid constructor 
    /// </summary>
    /// <param name="grid">The DSS grid object that has the information</param>
    /// <param name="dataRetrieved">Whether you retrieved data in ZgRetrieve or not</param>
    internal Grid(NativeSpatialGridWrapper grid, bool dataRetrieved)
    {
      _grid = grid;
      _data = _grid.Data;
    }

    private string ConvertMilitaryTimeTo12Hour(string timeString)
    {
      string part1 = timeString.Substring(0, 2);
      string AMOrPM = "AM";
      int p1;
      Int32.TryParse(part1, out p1);
      if (p1 > 12)
      {
        AMOrPM = "PM";
        p1 -= 12;
      }
      string part2 = timeString.Substring(2);
      return p1 + ":" + part2 + AMOrPM;
    }
  }

}
