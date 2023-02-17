using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

internal class DssNative
{
  [DllImport("hecdss")]
  public static extern int hec_dss_CONSTANT_MAX_PATH_SIZE();

  [DllImport("hecdss")]
   public static extern int hec_dss_open(string fileName, out IntPtr dss);

   [DllImport("hecdss")]
   public static extern int hec_dss_close(IntPtr dss);

   [DllImport("hecdss")]
   internal static extern int hec_dss_version(IntPtr dss);

  [DllImport("hecdss")]
  internal static extern int hec_dss_set_value(string name, int value);

  [DllImport("hecdss")]
  internal static extern int hec_dss_set_string(string name, string value);


  [DllImport("hecdss")]
  internal static extern int hec_dss_squeeze(string fileName);

  [DllImport("hecdss")]
  internal static extern int hec_dss_delete(IntPtr dss, string path);


  [DllImport("hecdss")]
   public static extern int hec_dss_record_count(IntPtr dss);

   [DllImport("hecdss")]
   public static extern IntPtr hec_dss_deprecated_ifltab(IntPtr dss);

   [DllImport("hecdss")]
   public static extern void hec_dss_deprecated_ifltab_print(IntPtr ifltab);

  [DllImport("hecdss")]
  public static extern int hec_dss_dataType(IntPtr dss, string pathname);


  [DllImport("hecdss")]
  public static extern int hec_dss_tsStoreRegular(IntPtr dss, string pathname,
            string startDate, string startTime,
            double[] valueArray, int arraySize,
            int[] qualityArray, int qualityArraySize,
            int saveAsFloat,
            byte[] units, byte[] type);

  [DllImport("hecdss")]
  public static extern int hec_dss_tsStoreIregular(IntPtr dss, string pathname,
  string startDateBase,
  int[] times, int timeGranularitySeconds,
  double[] valueArray, int valueArraySize,
  int[] qualityArray, int qualityArraySize,
  int saveAsFloat,
  byte[] units, byte[] type);

   [DllImport("hecdss", CharSet = CharSet.Ansi, ExactSpelling = true)]
   public static extern int hec_dss_tsRetrieve(IntPtr dss, string pathname,
                                     string startDate, string startTime,
                                     string endDate, string endTime,
                                     int[] timeArray, double[] valueArray, int arraySize,
                                     ref int numberValuesRead,int[] quality,ref int qualityLength, 
                                     ref int julianBaseDate, ref int timeGranularitySeconds,
                                     byte[] units, int unitsLength, byte[] type, int typeLength);


  [DllImport("hecdss")]
  internal static extern int hec_dss_tsGetDateTimeRange(IntPtr dss, string pathname, int boolFullSet,
                                            ref int firstValidJulian, ref int firstSeconds,
                                            ref int lastValidJulian, ref int lastSeconds);

   [DllImport("hecdss", CharSet = CharSet.Ansi, ExactSpelling = true)] 
   public static extern int hec_dss_tsGetSizes(IntPtr dss, string pathname,
                                    string startDate, string startTime, 
                                    string endDate, string endTime,
                                    ref int numberValues, ref int qualityElementSize);

  [DllImport("hecdss")]
  public static extern int hec_dss_tsRetrieveInfo(IntPtr dss, string pathname,
                          byte[] units, int unitsLength,
                          byte[] type, int typeLength);


  [DllImport("hecdss")]
  public static extern int hec_dss_catalog(IntPtr dss, byte[] pathBuffer,int[] recordTypes,[In] byte[] pathFilter,
    int count, int pathBufferItemSize);

  [DllImport("hecdss")]
  public static extern int hec_dss_dateToYearMonthDay(string date,ref int year, ref int month, ref int day);

  [DllImport("hecdss")]
  public static extern int hec_dss_dateToJulian(string date);

  [DllImport("hecdss")]
  public static extern int hec_dss_julianToYearMonthDay(int julian, ref int year, ref int month, ref int day);


  [DllImport("hecdss")]
  public static extern int hec_dss_locationRetrieve(IntPtr dss, string fullPath, ref double x, ref double y, ref double z,
                            ref int coordinateSystem, ref int coordinateID,
                            ref int horizontalUnits, ref int horizontalDatum,
                            ref int verticalUnits, ref int verticalDatum,
                            byte[] timeZoneName, int timeZoneNameLength,
                            byte[] supplemental, int supplementalLength);

  [DllImport("hecdss")]
  public static extern int hec_dss_locationStore(IntPtr dss, string fullPath,
  double x,  double y,  double z,
   int coordinateSystem,  int coordinateID,
   int horizontalUnits,  int horizontalDatum,
   int verticalUnits,  int verticalDatum,
   byte[] timeZoneName,
   byte[] supplemental,
   int replace);

  [DllImport("hecdss")] 
  public static extern int hec_dss_pdRetrieveInfo(IntPtr dss, string pathname,
                            ref int numberOrdinates, ref int numberCurves,
                            byte[] unitsIndependent, int unitsIndependentLength,
                            byte[] unitsDependent, int unitsDependentLength,
                            byte[] typeIndependent, int typeIndependentLength,
                            byte[] typeDependent, int typeDependentLength,
                            ref int labelsLength);

  [DllImport("hecdss")]
  public static extern int hec_dss_pdRetrieve(IntPtr dss, string pathname,
  double[] Ordinates, int OrdinatesLength,
  double[] Values, int ValuesLength,
  ref int numberOrdinates, ref int numberCurves,
  byte[] unitsIndependent, int unitsIndependentLength,
  byte[] typeIndependent, int typeIndependentLength,
  byte[] unitsDependent, int unitsDependentLength,
  byte[] typeDependent, int typeDependentLength,
  byte[] labels, int labelsLength);


  [DllImport("hecdss")]
  public static extern int hec_dss_gridRetrieve(IntPtr dss, string pathname, int boolRetrieveData,
  ref int type, ref int dataType,
  ref int lowerLeftCellX, ref int lowerLeftCellY,
  ref int numberOfCellsX, ref int numberOfCellsY,
  ref int numberOfRanges, ref int srsDefinitionType,
  ref int timeZoneRawOffset, ref int isInterval,
  ref int isTimeStamped,
  byte[] dataUnits, int dataUnitsLength,
  byte[] dataSource, int dataSourceLength,
  byte[] srsName, int srsNameLength,
  byte[] srsDefinition, int srsDefinitionLength,
  byte[] timeZoneID, int timeZoneIDLength,
  ref float cellSize, ref float xCoordOfGridCellZero,
  ref float yCoordOfGridCellZero, ref float nullValue,
  ref float maxDataValue, ref float minDataValue,
  ref float meanDataValue,
  float[] rangeLimitTable, int rangeTablesLength,
  int[] numberEqualOrExceedingRangeLimit,
  float[] data, int dataLength);

}

