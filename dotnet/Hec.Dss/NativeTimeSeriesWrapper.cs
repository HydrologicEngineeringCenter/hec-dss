using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;
using Hec.Dss.Native;

namespace Hec.Dss
{
  public class NativeTimeSeriesWrapper
  {
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    [return: MarshalAs(UnmanagedType.BStr)]
    public extern static string GetPathName(IntPtr ts);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    [return: MarshalAs(UnmanagedType.BStr)]
    public extern static string GetUnits(IntPtr ts);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    [return: MarshalAs(UnmanagedType.BStr)]
    public extern static string GetType(IntPtr ts);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    [return: MarshalAs(UnmanagedType.BStr)]
    public extern static string GetProgramName(IntPtr ts);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static IntPtr GetQuality(IntPtr ts);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static IntPtr GetDoubleValues(IntPtr ts);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static double GetTimeGranularitySeconds(IntPtr ts);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static double GetJulianBaseDate(IntPtr ts);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static IntPtr GetTimes(IntPtr ts);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static IntPtr GetDoubleProfileDepths(IntPtr ts);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static int GetProfileDepthsNumber(IntPtr ts);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static IntPtr GetDoubleProfileValues(IntPtr ts);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static int GetNumberValues(IntPtr ts);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    public extern static IntPtr GetLocation(IntPtr ts);


    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetQuality(IntPtr ts, int[] quality, int arraySize);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetQualityElementSize(IntPtr ts, int elementSize);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetDoubleProfileDepths(IntPtr ts, double[] values, int arrayLength);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetProfileDepthsNumber(IntPtr ts, int value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetDoubleProfileValues(IntPtr ts, double[] values, int arrayLength);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetNumberValues(IntPtr ts, int value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetUnitsProfileValues(IntPtr ts, string value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetUnitsProfileDepths(IntPtr ts, string value);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetFloatProfileDepths(IntPtr ts, float[] values, int arrayLength);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetFloatProfileValues(IntPtr ts, float[] values, int arrayLength);

    public IntPtr TheStruct;

    public int[] Quality 
    { 
      get
      {
        IntPtr ptr = GetQuality(TheStruct);
        if (ptr != IntPtr.Zero)
        {
          int[] r = new int[NumberValues];
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
        SetQuality(TheStruct, value, value.Length);
      }  
    }
    public int QualityElementSize
    {
      set
      {
        SetQualityElementSize(TheStruct, value);
      }
    }

    public string Path
    {
      get
      {
        return GetPathName(TheStruct);
      }
    }
    public string Units
    {
      get
      {
        return GetUnits(TheStruct);
      }
    }
    public string Type
    {
      get
      {
        return GetType(TheStruct);
      }
    }
    public string ProgramName
    {
      get
      {
        return GetProgramName(TheStruct);
      }
    }
    public NativeLocationWrapper LocationStruct
    {
      get
      {
        NativeLocationWrapper nlw = new NativeLocationWrapper();
        nlw.TheStruct = GetLocation(TheStruct);
        return nlw;
      }
    }
    public double[] DoubleValues
    {
      get
      {
        IntPtr ptr = GetDoubleValues(TheStruct);
        if (ptr != IntPtr.Zero)
        {
          double[] r = new double[NumberValues];
          Marshal.Copy(ptr, r, 0, r.Length);
          return r;
        }
        else
        {
          return null;
        }
      }
    }
    public double TimeGranularitySeconds
    {
      get
      {
        return GetTimeGranularitySeconds(TheStruct);
      }
    }
    public double JulianBaseDate
    {
      get
      {
        return GetJulianBaseDate(TheStruct);
      }
    }
    public int[] Times
    {
      get
      {
        IntPtr ptr = GetTimes(TheStruct);
        if (ptr != IntPtr.Zero)
        {
          int[] r = new int[NumberValues];
          Marshal.Copy(ptr, r, 0, r.Length);
          return r;
        }
        else
        {
          return null;
        }
      }
    }
    public double[] DoubleProfileDepths
    {
      get
      {
        IntPtr ptr = GetDoubleProfileDepths(TheStruct);
        if (ptr != IntPtr.Zero)
        {
          double[] r = new double[NumberValues];
          Marshal.Copy(ptr, r, 0, r.Length);
          return new ArraySegment<double>(r, 0, ProfileDepthsNumber).ToArray<double>();
        }
        else
        {
          return null;
        }
      }
      set
      {
        SetDoubleProfileDepths(TheStruct, value, value.Length);
      }
    }
    public int ProfileDepthsNumber
    {
      get
      {
        return GetProfileDepthsNumber(TheStruct);
      }
      set
      {
        SetProfileDepthsNumber(TheStruct, value);
      }
    }
    public double[] DoubleProfileValues
    {
      get
      {
        IntPtr ptr = GetDoubleProfileValues(TheStruct);
        if (ptr != IntPtr.Zero)
        {
          int size = ProfileDepthsNumber * NumberValues;
          double[] r = new double[size];
          Marshal.Copy(ptr, r, 0, size);
          return r;
        }
        else
        {
          return null;
        }
      }
      set
      {
        SetDoubleProfileValues(TheStruct, value, value.Length);
      }
    }
    public int NumberValues
    {
      get
      {
        return GetNumberValues(TheStruct);
      }
      set
      {
        SetNumberValues(TheStruct, value);
      }
    }

    public string UnitsProfileValues
    {
      set
      {
        SetUnitsProfileValues(TheStruct, value);
      }
    }
    public string UnitsProfileDepths
    {
      set
      {
        SetUnitsProfileDepths(TheStruct, value);
      }
    }
    public float[] FloatProfileDepths
    {
      set
      {
        SetFloatProfileDepths(TheStruct, value, value.Length);
      }
    }
    public float[] FloatProfileValues
    {
      set
      {
        SetFloatProfileValues(TheStruct, value, value.Length);
      }
    }
  }
}
