using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;
using Hec.Dss.Native;

namespace Hec.Dss
{
  public static class PInvoke
  {
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static int ZOpen(long[] ifltab, string dssFilename);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static int ZSet(string parameter, string charVal, int integerValue);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static void ZSetMessageLevel(int methodId, int levelId);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static int ZGetVersion(long[] ifltab);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static int ZClose(long[] ifltab);


    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int ZTsStore(long[] ifltab, IntPtr tss, int storageFlag);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static IntPtr ZStructTsNewRegDoubles(string pathName, double[] dArray, int numberValues, string startDate, string startTime, string units, string type);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static IntPtr ZStructTsNewRegFloats(string pathName, float[] fArray, int numberValues, string startDate, string startTime, string units, string type);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static IntPtr ZStructTsNewIrregDoubles(string pathName, double[] dArray, int numberValues, int[] itimes, int timeGranularitySeconds, string startDateBase, string units, string type);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static IntPtr ZStructTsNewIrregFloats(string pathName, float[] fArray, int numberValues, int[] itimes, int timeGranularitySeconds, string startDateBase, string units, string type);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static IntPtr ZStructTsNewTimes(string pathName, string startDate, string startTime, string endDate, string endTime);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static IntPtr ZStructTsNew(string pathName);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int ZTsRetrieve(long[] ifltab, IntPtr tss, int retrieveFlag, int boolRetrieveDoubles, int boolRetrieveQualityNotes);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static IntPtr ZStructCatalogNew();
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int ZCatalog(long[] ifltab, string pathWithWild, IntPtr cat, int boolSorted);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int ZTsRetrieveEmpty(long[] ifltab, IntPtr tss);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static IntPtr ZLocationRetrieve(long[] ifltab, StringBuilder pathName);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static int ZDataType(long[] ifltab, string pathName);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static int ZSqueeze(string pathName);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static int ZSqueeze7(long[] ifltab, int boolOnlyIfNeeded, int boolInPlace);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static int ZSqueezeNeeded(long[] ifltab);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static int GetDateAndTime(int timeMinOrSec, int timeGranularitySeconds, int julianBaseDate, StringBuilder dateString, int sizeOfDateString, StringBuilder hoursMins, int sizeOfHoursMins);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static int ZCheck(long[] ifltab, string pathName);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static int ZDelete(long[] ifltab, string pathName);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static float ZMissingFlag();
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static IntPtr ZStructLocationNew(string pathName);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int ZLocationStore(long[] ifltab, IntPtr loc, int storageFlag);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int ZPathNameForm(string aPart, string bPart, string cPart, string dPart, string ePart, string fPart, StringBuilder pathName, ulong sizeOfPathName);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static int JulianToYearMonthDay(int julian, ref int year, ref int month, ref int day);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static int TimeStringToSeconds(string timeString);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static int DateToJulian(string dateString);

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

    public static int GetDateAndTime(int timeMinOrSec, int timeGranularitySeconds, int julianBaseDate, string dateString, int sizeOfDateString, string hoursMins, int sizeOfHoursMins)
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
