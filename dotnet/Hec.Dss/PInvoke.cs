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
  }
}
