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
    [DllImport("hecdss")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetSpatialGridPathName(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetSpatialGridPathName(IntPtr grid, string value);
    [DllImport("hecdss")]
    private extern static int GetGridType(IntPtr grid);
    [DllImport("hecdss")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetDataUnits(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetDataUnits(IntPtr grid, string value);
    [DllImport("hecdss")]
    private extern static int GetDataType(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetDataType(IntPtr grid, int value);
    [DllImport("hecdss")]
    private extern static int GetLowerLeftCellX(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetLowerLeftCellX(IntPtr grid, int value);
    [DllImport("hecdss")]
    private extern static int GetLowerLeftCellY(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetLowerLeftCellY(IntPtr grid, int value);
    [DllImport("hecdss")]
    private extern static int GetNumberOfCellsX(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetNumberOfCellsX(IntPtr grid, int value);
    [DllImport("hecdss")]
    private extern static int GetNumberOfCellsY(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetNumberOfCellsY(IntPtr grid, int value);
    [DllImport("hecdss")]
    private extern static float GetCellSize(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetCellSize(IntPtr grid, float value);
    [DllImport("hecdss")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetSRSName(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetSRSName(IntPtr grid, string value);
    [DllImport("hecdss")]
    private extern static int GetSRSDefinitionType(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetSRSDefinitionType(IntPtr grid, int value);
    [DllImport("hecdss")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetSRSDefinition(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetSRSDefinition(IntPtr grid, string value);
    [DllImport("hecdss")]
    private extern static float GetXCoordOfGridCellZero(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetXCoordOfGridCellZero(IntPtr grid, float value);
    [DllImport("hecdss")]
    private extern static float GetYCoordOfGridCellZero(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetYCoordOfGridCellZero(IntPtr grid, float value);
    [DllImport("hecdss")]
    private extern static int GetStructVersion(IntPtr grid);
    [DllImport("hecdss")]
    private extern static int GetSpatialGridVersion(IntPtr grid);
    [DllImport("hecdss")]
    private extern static int GetStorageDataType(IntPtr grid);
    [DllImport("hecdss")]
    private extern static int GetCompressionMethod(IntPtr grid);
    [DllImport("hecdss")]
    private extern static int GetSizeOfCompressedElements(IntPtr grid);
    [DllImport("hecdss")]
    private extern static int GetNumberOfRanges(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetNumberOfRanges(IntPtr grid, int value);
    [DllImport("hecdss")]
    private extern static float GetNullValue(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetNullValue(IntPtr grid, float value);
    [DllImport("hecdss")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetTimeZoneID(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetTimeZoneID(IntPtr grid, string value);
    [DllImport("hecdss")]
    private extern static int GetTimeZoneRawOffset(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetTimeZoneRawOffset(IntPtr grid, int value);
    [DllImport("hecdss")]
    private extern static bool GetIsInterval(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetIsInterval(IntPtr grid, bool value);
    [DllImport("hecdss")]
    private extern static bool GetIsTimeStamped(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetIsTimeStamped(IntPtr grid, bool value);
    [DllImport("hecdss")]
    private extern static float GetMaxDataValue(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetMaxDataValue(IntPtr grid, float value);
    [DllImport("hecdss")]
    private extern static float GetMinDataValue(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetMinDataValue(IntPtr grid, float value);
    [DllImport("hecdss")]
    private extern static float GetMeanDataValue(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetMeanDataValue(IntPtr grid, float value);
    [DllImport("hecdss")]
    private extern static IntPtr GetRangeLimitTable(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetRangeLimitTable(IntPtr grid, float[] value);
    [DllImport("hecdss")]
    private extern static float GetUndefinedValue(IntPtr grid);
    [DllImport("hecdss")]
    private extern static IntPtr GetNumberEqualOrExceedingRangeLimit(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetNumberEqualOrExceedingRangeLimit(IntPtr grid, int[] value);
    [DllImport("hecdss")]
    private extern static IntPtr GetData(IntPtr grid);
    [DllImport("hecdss")]
    private extern static void SetData(IntPtr grid, float[] value);


    public IntPtr TheStruct { get; internal set; }
    public string PathName
    {
      get
      {
        return GetSpatialGridPathName(TheStruct);
      }
      set
      {
        SetSpatialGridPathName(TheStruct, value);
      }
    }
    public int GridType
    {
      get
      {
        return GetGridType(TheStruct);
      }
    }
    public string DataUnits
    {
      get
      {
        return GetDataUnits(TheStruct);
      }
      set
      {
        SetDataUnits(TheStruct, value);
      }
    }
    public int DataType
    {
      get
      {
        return GetDataType(TheStruct);
      }
      set
      {
        SetDataType(TheStruct, value);
      }
    }
    public int LowerLeftCellX
    {
      get
      {
        return GetLowerLeftCellX(TheStruct);
      }
      set
      {
        SetLowerLeftCellX(TheStruct, value);
      }
    }
    public int LowerLeftCellY {
      get
      {
        return GetLowerLeftCellY(TheStruct);
      }
      set
      {
        SetLowerLeftCellY(TheStruct, value);
      }
    }
    public int NumberOfCellsX {
      get
      {
        return GetNumberOfCellsX(TheStruct);
      }
      set
      {
        SetNumberOfCellsX(TheStruct, value);
      }
    }
    public int NumberOfCellsY
    {
      get
      {
        return GetNumberOfCellsY(TheStruct);
      }
      set
      {
        SetNumberOfCellsY(TheStruct, value);
      }
    }
    public float CellSize {
      get
      {
        return GetCellSize(TheStruct);
      }
      set
      {
        SetCellSize(TheStruct, value);
      }
    }
    public string SRSName
    {
      get
      {
        return GetSRSName(TheStruct);
      }
      set
      {
        SetSRSName(TheStruct, value);
      }
    }
    public int SRSDefinitionType
    {
      get
      {
        return GetSRSDefinitionType(TheStruct);
      }
      set
      {
        SetSRSDefinitionType(TheStruct, value);
      }
    }
    public string SRSDefinition
    {
      get
      {
        return GetSRSDefinition(TheStruct);
      }
      set
      {
        SetSRSDefinition(TheStruct, value);
      }
    }
    public float XCoordOfGridCellZero {
      get
      {
        return GetXCoordOfGridCellZero(TheStruct);
      }
      set
      {
        SetXCoordOfGridCellZero(TheStruct, value);
      }
    }
    public float YCoordOfGridCellZero
    {
      get
      {
        return GetYCoordOfGridCellZero(TheStruct);
      }
      set
      {
        SetYCoordOfGridCellZero(TheStruct, value);
      }
    }
    public int StructVersion
    {
      get
      {
        return GetStructVersion(TheStruct);
      }
    }
    public int Version
    {
      get
      {
        return GetSpatialGridVersion(TheStruct);
      }
    }
    public int StorageDataType
    {
      get
      {
        return GetStorageDataType(TheStruct);
      }
    }
    public int CompressionMethod
    {
      get
      {
        return GetCompressionMethod(TheStruct);
      }
    }
    public int SizeOfCompressedElements
    {
      get
      {
        return GetSizeOfCompressedElements(TheStruct);
      }
    }
    public int NumberOfRanges
    {
      get
      {
        return GetNumberOfRanges(TheStruct);
      }
      set
      {
        SetNumberOfRanges(TheStruct, value);
      }
    }
    public float NullValue
    {
      get
      {
        return GetNullValue(TheStruct);
      }
      set
      {
        SetNullValue(TheStruct, value);
      }
    }
    public string TimeZoneID
    {
      get
      {
        return GetTimeZoneID(TheStruct);
      }
      set
      {
        SetTimeZoneID(TheStruct, value);
      }
    }
    public int TimeZoneRawOffset
    {
      get
      {
        return GetTimeZoneRawOffset(TheStruct);
      }
      set
      {
        SetTimeZoneRawOffset(TheStruct, value);
      }
    }
    public bool IsInterval
    {
      get
      {
        return GetIsInterval(TheStruct);
      }
      set
      {
        SetIsInterval(TheStruct, value);
      }
    }
    public bool IsTimeStamped
    {
      get
      {
        return GetIsTimeStamped(TheStruct);
      }
      set
      {
        SetIsTimeStamped(TheStruct, value);
      }
    }
    public float MaxDataValue
    {
      get
      {
        return GetMaxDataValue(TheStruct);
      }
      set
      {
        SetMaxDataValue(TheStruct, value);
      }
    }
    public float MinDataValue
    {
      get
      {
        return GetMinDataValue(TheStruct);
      }
      set
      {
        SetMinDataValue(TheStruct, value);
      }
    }
    public float MeanDataValue
    {
      get
      {
        return GetMeanDataValue(TheStruct);
      }
      set
      {
        SetMeanDataValue(TheStruct, value);
      }
    }
    public float[] RangeLimitTable
    {
      get
      {
        IntPtr ptr = GetRangeLimitTable(TheStruct);
        if (ptr != IntPtr.Zero)
        {
          float[] r = new float[NumberOfRanges];
          Marshal.Copy(ptr, r, 0, r.Length);
          return r;
        }
        else
        {
          return null;
        }
      }
      set
      {
        SetRangeLimitTable(TheStruct, value);
      }
    }
    public float UndefinedValue
    {
      get
      {
        return GetUndefinedValue(TheStruct);
      }
    }
    public int[] NumberEqualOrExceedingRangeLimit
    {
      get
      {
        IntPtr ptr = GetNumberEqualOrExceedingRangeLimit(TheStruct);
        if (ptr != IntPtr.Zero)
        {
          int[] r = new int[NumberOfRanges];
          Marshal.Copy(ptr, r, 0, r.Length);
          return r;
        }
        else
        {
          return null;
        }
      }
      set
      {
        SetNumberEqualOrExceedingRangeLimit(TheStruct, value);
      }
    }
    public float[] Data
    {
      get
      {
        IntPtr ptr = GetData(TheStruct);
        if (ptr != IntPtr.Zero)
        {
          float[] r = new float[NumberOfCellsX * NumberOfCellsY];
          Marshal.Copy(ptr, r, 0, r.Length);
          return r;
        }
        else
        {
          return null;
        }
      }
      set
      {
        SetData(TheStruct, value);
      }
    }
  }
}
