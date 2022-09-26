using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

public class DssNative
{
   [DllImport("hecdss")]
   public static extern IntPtr hec_dss_open(string fileName);

   [DllImport("hecdss")]
   public static extern int hec_dss_open(string fileName, out IntPtr dss);

   [DllImport("hecdss")]
   public static extern int hec_dss_close(IntPtr dss);

   [DllImport("hecdss")]
   public static extern IntPtr hec_dss_deprecated_ifltab(IntPtr dss);

   [DllImport("hecdss")]
   public static extern void hec_dss_deprecated_ifltab_print(IntPtr ifltab);


   [DllImport("hecdss", CharSet = CharSet.Ansi, ExactSpelling = true)]
   public static extern int hec_dss_tsRetrieve(IntPtr dss, string pathname,
                                     string startDate, string startTime,
                                     string endDate, string endTime,
                                     int[] timeArray, double[] valueArray, int arraySize,
                                     ref int numberValuesRead, ref int julianBaseDate,
                                     byte[] units, int unitsLength, byte[] type, int typeLength);


  [DllImport("hecdss")]
  internal static extern int hec_dss_tsGetDateTimeRange(IntPtr dss, string pathname, int boolFullSet,
                                            ref int firstValidJulian, ref int firstSeconds,
                                            ref int lastValidJulian, ref int lastSeconds);

   [DllImport("hecdss", CharSet = CharSet.Ansi, ExactSpelling = true)] 
   public static extern int hec_dss_tsGetSizes(IntPtr dss, string pathname,
                                    string startDate, string startTime, 
                                    string endDate, string endTime,
                                    ref int numberValues);

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
  public static extern int hec_dss_pdRetrieveInfo(IntPtr dss, string pathname,
                            ref int numberOrdinates, ref int numberCurves,
                            byte[] unitsIndependent, int unitsIndependentLength,
                            byte[] unitsDependent, int unitsDependentLength,
                            byte[] typeIndependent, int typeIndependentLength,
                            byte[] typeDependent, int typeDependentLength);

}

