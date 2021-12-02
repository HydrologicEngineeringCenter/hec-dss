using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace Hec.Dss
{
  public static class PInvoke
  {
    [DllImport("hecdss")]
    public extern static int ZOpen(long[] ifltab, string dssFilename);
    [DllImport("hecdss")]
    public extern static int ZSet(string parameter, string charVal, int integerValue);
    [DllImport("hecdss")]
    public extern static void ZSetMessageLevel(int methodId, int levelId);
    [DllImport("hecdss")]
    public extern static int ZGetVersion(long[] ifltab);
    [DllImport("hecdss")]
    public extern static int ZClose(long[] ifltab);
    [DllImport("hecdss")]
    private extern static int ZTsStore(long[] ifltab, IntPtr tss, int storageFlag);
    [DllImport("hecdss")]
    private extern static IntPtr ZStructTsNewRegDoubles(string pathName, double[] dArray, int numberValues, string startDate, string startTime, string units, string type);
    [DllImport("hecdss")]
    private extern static IntPtr ZStructTsNewRegFloats(string pathName, float[] fArray, int numberValues, string startDate, string startTime, string units, string type);
    [DllImport("hecdss")]
    private extern static IntPtr ZStructTsNewIrregDoubles(string pathName, double[] dArray, int numberValues, int[] itimes, int timeGranularitySeconds, string startDateBase, string units, string type);
    [DllImport("hecdss")]
    private extern static IntPtr ZStructTsNewIrregFloats(string pathName, float[] fArray, int numberValues, int[] itimes, int timeGranularitySeconds, string startDateBase, string units, string type);
    [DllImport("hecdss")]
    private extern static IntPtr ZStructTsNewTimes(string pathName, string startDate, string startTime, string endDate, string endTime);
    [DllImport("hecdss")]
    private extern static IntPtr ZStructTsNew(string pathName);
    [DllImport("hecdss")]
    private extern static int ZTsRetrieve(long[] ifltab, IntPtr tss, int retrieveFlag, int boolRetrieveDoubles, int boolRetrieveQualityNotes);
    [DllImport("hecdss")]
    private extern static IntPtr ZStructCatalogNew();
    [DllImport("hecdss")]
    private extern static int ZCatalog(long[] ifltab, string pathWithWild, IntPtr cat, int boolSorted);
    [DllImport("hecdss")]
    private extern static int ZTsRetrieveEmpty(long[] ifltab, IntPtr tss);
    [DllImport("hecdss")]
    private extern static IntPtr ZLocationRetrieve(long[] ifltab, StringBuilder pathName);
    [DllImport("hecdss")]
    public extern static int ZDataType(long[] ifltab, string pathName);
    [DllImport("hecdss")]
    public extern static int ZSqueeze(string pathName);
    [DllImport("hecdss")]
    public extern static int ZSqueeze7(long[] ifltab, int boolOnlyIfNeeded, int boolInPlace);
    [DllImport("hecdss")]
    public extern static int ZSqueezeNeeded(long[] ifltab);
    [DllImport("hecdss")]
    private extern static int GetDateAndTime(int timeMinOrSec, int timeGranularitySeconds, int julianBaseDate, StringBuilder dateString, int sizeOfDateString, StringBuilder hoursMins, int sizeOfHoursMins);
    [DllImport("hecdss")]
    public extern static int ZCheck(long[] ifltab, string pathName);
    [DllImport("hecdss")]
    public extern static int ZDelete(long[] ifltab, string pathName);
    [DllImport("hecdss")]
    public extern static float ZMissingFlag();
    [DllImport("hecdss")]
    private extern static IntPtr ZStructLocationNew(string pathName);
    [DllImport("hecdss")]
    private extern static int ZLocationStore(long[] ifltab, IntPtr loc, int storageFlag);
    [DllImport("hecdss")]
    private extern static int ZPathNameForm(string aPart, string bPart, string cPart, string dPart, string ePart, string fPart, StringBuilder pathName, ulong sizeOfPathName);
    [DllImport("hecdss")]
    public extern static int JulianToYearMonthDay(int julian, ref int year, ref int month, ref int day);
    [DllImport("hecdss")]
    public extern static int TimeStringToSeconds(string timeString);
    [DllImport("hecdss")]
    public extern static int DateToJulian(string dateString);
    [DllImport("hecdss")]
    private extern static IntPtr ZStructPdNew(string pathName);
    [DllImport("hecdss")]
    private extern static IntPtr ZStructPdNewDoubles(string pathName, double[] doubleOrdinates, double[] doubleValues, int numberOrdinates, int numberCurves, string unitsIndependent, string typeIndependent, string unitsDependent, string typeDependent);
    [DllImport("hecdss")]
    private extern static IntPtr ZStructPdNewFloats(string pathName, float[] floatOrdinates, float[] floatValues, int numberOrdinates, int numberCurves, string unitsIndependent, string typeIndependent, string unitsDependent, string typeDependent);
    [DllImport("hecdss")]
    private extern static int ZPdRetrieve(long[] ifltab, IntPtr pd, int retrieveDoubleFlag);
    [DllImport("hecdss")]
    private extern static int ZPdStore(long[] ifltab, IntPtr pd, int storageFlag);
    [DllImport("hecdss")]
    private extern static IntPtr ZStructSpatialGridNew(string pathName);
    [DllImport("hecdss")]
    private extern static int ZSpatialGridRetrieve(long[] ifltab, IntPtr grid, bool retrieveData);
    [DllImport("hecdss")]
    private extern static int ZSpatialGridStore(long[] ifltab, IntPtr grid);
    [DllImport("hecdss")]
    public extern static int DateToYearMonthDay(string date, ref int year, ref int month, ref int day);
    [DllImport("hecdss")]
    public extern static bool IsTimeDefined(int julianDate, int timeSeconds);
    [DllImport("hecdss")]
    [return: MarshalAs(UnmanagedType.BStr)]
    public extern static string AlbersSRS();
    [DllImport("hecdss")]
    public extern static int YearMonthDayToJulian(int year, int month, int day);
    [DllImport("hecdss")]
    public extern static int ZCatalogFile(long[] ifltab, string pathWithWild, int boolSorted, string catalogFileName);

    public static int ZSpatialGridStore(long[] ifltab, NativeSpatialGridWrapper grid)
    {
      return ZSpatialGridStore(ifltab, grid.TheStruct);
    }

    public static int ZSpatialGridRetrieve(long[] ifltab, NativeSpatialGridWrapper grid, bool retrieveData)
    {
      return ZSpatialGridRetrieve(ifltab, grid.TheStruct, retrieveData);
    }

    public static NativeSpatialGridWrapper NativeSpatialGridNew(string pathName)
    {
      NativeSpatialGridWrapper grid = new NativeSpatialGridWrapper();
      grid.TheStruct = ZStructSpatialGridNew(pathName);
      return grid;
    }

    public static int ZPdStore(long[] ifltab, NativePairedDataWrapper pd, int storageFlag)
    {
      return ZPdStore(ifltab, pd.TheStruct, storageFlag);
    }

    public static int ZPdRetrieve(long[] ifltab, NativePairedDataWrapper pd, int retrieveDoubleFlag)
    {
      return ZPdRetrieve(ifltab, pd.TheStruct, retrieveDoubleFlag);
    }

    public static NativePairedDataWrapper NativePdNewFloats(string pathName, float[] floatOrdinates, float[] floatValues, int numberOrdinates, int numberCurves, string unitsIndependent, string typeIndependent, string unitsDependent, string typeDependent)
    {
      NativePairedDataWrapper npd = new NativePairedDataWrapper();
      npd.TheStruct = ZStructPdNewFloats(pathName, floatOrdinates, floatValues, numberOrdinates, numberCurves, unitsIndependent, typeIndependent, unitsDependent, typeDependent);
      return npd;
    }

    public static NativePairedDataWrapper NativePdNewDoubles(string pathName, double[] doubleOrdinates, double[] doubleValues, int numberOrdinates, int numberCurves, string unitsIndependent, string typeIndependent, string unitsDependent, string typeDependent)
    {
      NativePairedDataWrapper npd = new NativePairedDataWrapper();
      npd.TheStruct = ZStructPdNewDoubles(pathName, doubleOrdinates, doubleValues, numberOrdinates, numberCurves, unitsIndependent, typeIndependent, unitsDependent, typeDependent);
      return npd;
    }

    public static NativePairedDataWrapper NativePdNew(string pathName)
    {
      NativePairedDataWrapper npd = new NativePairedDataWrapper();
      npd.TheStruct = ZStructPdNew(pathName);
      return npd;
    }

    public static int ZPathNameForm(string aPart, string bPart, string cPart, string dPart, string ePart, string fPart, ref string pathName, ulong sizeOfPathName)
    {
      StringBuilder p = new StringBuilder((int)sizeOfPathName);
      int status = ZPathNameForm(aPart, bPart, cPart, dPart, ePart, fPart, p, sizeOfPathName);
      pathName = p.ToString();
      return status;
    }

    public static int ZLocationStore(long[] ifltab, NativeLocationWrapper loc, int storageFlag)
    {
      return ZLocationStore(ifltab, loc.TheStruct, storageFlag);
    }

    public static NativeLocationWrapper NativeLocationNew(string pathName)
    {
      NativeLocationWrapper loc = new NativeLocationWrapper();
      loc.TheStruct = ZStructLocationNew(pathName);
      return loc;
    }

    public static NativeCatalogWrapper NativeCatalogNew()
    {
      NativeCatalogWrapper ncw = new NativeCatalogWrapper();
      ncw.TheStruct = ZStructCatalogNew();
      return ncw;
    }

    public static int ZCatalog(long[] ifltab, string pathWithWIld, NativeCatalogWrapper cat, int boolSorted)
    {
      return ZCatalog(ifltab, pathWithWIld, cat.TheStruct, boolSorted);
    }

    public static NativeTimeSeriesWrapper NativeTsNewRegDoubles(string pathName, double[] dArray, string startDate, string startTime, string units, string type)
    {
      NativeTimeSeriesWrapper nts = new NativeTimeSeriesWrapper();
      nts.TheStruct = ZStructTsNewRegDoubles(pathName, dArray, dArray.Length, startDate, startTime, units, type);
      return nts;
    }

    public static int ZTsStore(long[] ifltab, NativeTimeSeriesWrapper tss, int storageFlag)
    {
      return ZTsStore(ifltab, tss.TheStruct, storageFlag);
    }

    public static NativeTimeSeriesWrapper NativeTsNewRegFloats(string pathName, float[] fArray, string startDate, string startTime, string units, string type)
    {
      NativeTimeSeriesWrapper nts = new NativeTimeSeriesWrapper();
      nts.TheStruct = ZStructTsNewRegFloats(pathName, fArray, fArray.Length, startDate, startTime, units, type);
      return nts;
    }

    public static NativeLocationWrapper ZLocationRetrieve(long[] ifltab, string fullPath)
    {
      NativeLocationWrapper nlw = new NativeLocationWrapper();
      nlw.TheStruct = ZLocationRetrieve(ifltab, new StringBuilder(fullPath));
      return nlw;
    }

    public static NativeTimeSeriesWrapper NativeTsNewIrregDoubles(string pathName, double[] dArray, int[] itimes, int timeGranularitySeconds, string startDateBase, string units, string type)
    {
      NativeTimeSeriesWrapper nts = new NativeTimeSeriesWrapper();
      nts.TheStruct = ZStructTsNewIrregDoubles(pathName, dArray, dArray.Length, itimes, timeGranularitySeconds, startDateBase, units, type);
      return nts;
    }

    public static NativeTimeSeriesWrapper NativeTsNewIrregFloats(string pathName, float[] fArray, int[] itimes, int timeGranularitySeconds, string startDateBase, string units, string type)
    {
      NativeTimeSeriesWrapper nts = new NativeTimeSeriesWrapper();
      nts.TheStruct = ZStructTsNewIrregFloats(pathName, fArray, fArray.Length, itimes, timeGranularitySeconds, startDateBase, units, type);
      return nts;
    }

    public static NativeTimeSeriesWrapper NativeTsNewTimes(string fullPath, string startDate, string startTime, string endDate, string endTime)
    {
      NativeTimeSeriesWrapper nts = new NativeTimeSeriesWrapper();
      nts.TheStruct = ZStructTsNewTimes(fullPath, startDate, startTime, endDate, endTime);
      return nts;
    }

    public static NativeTimeSeriesWrapper NativeTsNew(string fullPath)
    {
      NativeTimeSeriesWrapper nts = new NativeTimeSeriesWrapper();
      nts.TheStruct = ZStructTsNew(fullPath);
      return nts;
    }

    public static int ZTsRetrieve(long[] ifltab, NativeTimeSeriesWrapper blockTimeSeries, int retrieveFlag, int retrieveDoublesFlag, int boolRetrieveQualityNotes)
    {
      return ZTsRetrieve(ifltab, blockTimeSeries.TheStruct, retrieveFlag, retrieveDoublesFlag, boolRetrieveQualityNotes);
    }

    public static int ZTsRetrieveEmpty(long[] ifltab, NativeTimeSeriesWrapper tss)
    {
      return ZTsRetrieveEmpty(ifltab, tss.TheStruct);
    }

    public static int GetDateAndTime(int timeMinOrSec, int timeGranularitySeconds, int julianBaseDate, out string dateString, int sizeOfDateString, out string hoursMins, int sizeOfHoursMins)
    {
      StringBuilder dateStringBuilder = new StringBuilder(sizeOfDateString);
      StringBuilder hoursMinsStringBuilder = new StringBuilder(sizeOfHoursMins);
      int status = GetDateAndTime(timeMinOrSec, timeGranularitySeconds, julianBaseDate, dateStringBuilder, sizeOfDateString, hoursMinsStringBuilder, sizeOfHoursMins);
      dateString = dateStringBuilder.ToString();
      hoursMins = hoursMinsStringBuilder.ToString();
      return status;
    }
  }
}
