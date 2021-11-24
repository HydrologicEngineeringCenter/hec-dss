using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace Hec.Dss
{
  public class NativeSpatialGridWrapper
  {
    public IntPtr TheStruct { get; internal set; }
    public string PathName { get; internal set; }
    public int GridType { get; internal set; }
    public string DataUnits { get; internal set; }
    public int DataType { get; internal set; }
    public int LowerLeftCellX { get; internal set; }
    public int LowerLeftCellY { get; internal set; }
    public int NumberOfCellsX { get; internal set; }
    public int NumberOfCellsY { get; internal set; }
    public float CellSize { get; internal set; }
    public string SRSName { get; internal set; }
    public int SRSDefinitionType { get; internal set; }
    public string SRSDefinition { get; internal set; }
    public float XCoordOfGridCellZero { get; internal set; }
    public float YCoordOfGridCellZero { get; internal set; }
    public int StructVersion { get; internal set; }
    public int Version { get; internal set; }
    public int StorageDataType { get; internal set; }
    public int CompressionMethod { get; internal set; }
    public int SizeOfCompressedElements { get; internal set; }
    public int NumberOfRanges { get; internal set; }
    public float NullValue { get; internal set; }
    public string TimeZoneID { get; internal set; }
    public int TimeZoneRawOffset { get; internal set; }
    public bool IsInterval { get; internal set; }
    public bool IsTimeStamped { get; internal set; }
    public float MaxDataValue { get; internal set; }
    public float MinDataValue { get; internal set; }
    public float MeanDataValue { get; internal set; }
    public float[] RangeLimitTable { get; internal set; }
    public float UndefinedValue { get; internal set; }
    public int[] NumberEqualOrExceedingRangeLimit { get; internal set; }
    public float[] Data { get; internal set; }
  }
}
