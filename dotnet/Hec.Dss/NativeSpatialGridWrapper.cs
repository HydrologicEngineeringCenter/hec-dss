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
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetPathName(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetPathName(IntPtr grid, string value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetGridType(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetDataUnits(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetDataUnits(IntPtr grid, string value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetDataType(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetDataType(IntPtr grid, int value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetLowerLeftCellX(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetLowerLeftCellX(IntPtr grid, int value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetLowerLeftCellY(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetLowerLeftCellY(IntPtr grid, int value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetNumberOfCellsX(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetNumberOfCellsX(IntPtr grid, int value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetNumberOfCellsY(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetNumberOfCellsY(IntPtr grid, int value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static float GetCellSize(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetCellSize(IntPtr grid, float value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetSRSName(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetSRSName(IntPtr grid, string value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetSRSDefinitionType(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetSRSDefinitionType(IntPtr grid, int value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetSRSDefinition(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetSRSDefinition(IntPtr grid, string value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static float GetXCoordOfGridCellZero(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetXCoordOfGridCellZero(IntPtr grid, float value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static float GetYCoordOfGridCellZero(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetYCoordOfGridCellZero(IntPtr grid, float value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetStructVersion(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetVersion(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetStorageDataType(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetCompressionMethod(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetSizeOfCompressedElements(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetNumberOfRanges(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetNumberOfRanges(IntPtr grid, int value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static float GetNullValue(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetNullValue(IntPtr grid, float value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetTimeZoneID(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetTimeZoneID(IntPtr grid, string value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetTimeZoneRawOffset(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetTimeZoneRawOffset(IntPtr grid, int value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static bool GetIsInterval(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetIsInterval(IntPtr grid, bool value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static bool GetIsTimeStamped(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetIsTimeStamped(IntPtr grid, bool value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static float GetMaxDataValue(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetMaxDataValue(IntPtr grid, float value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static float GetMinDataValue(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetMinDataValue(IntPtr grid, float value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static float GetMeanDataValue(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetMeanDataValue(IntPtr grid, float value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static IntPtr GetRangeLimitTable(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetRangeLimitTable(IntPtr grid, float[] value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static float GetUndefinedValue(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static IntPtr GetNumberEqualOrExceedingRangeLimit(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetNumberEqualOrExceedingRangeLimit(IntPtr grid, int[] value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static IntPtr GetData(IntPtr grid);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetData(IntPtr grid, float[] value);


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
