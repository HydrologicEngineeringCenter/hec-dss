using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace Hec.Dss
{
  public class NativeTimeSeriesWrapper
  {
    [DllImport("hecdss")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetPathName(IntPtr ts);
    [DllImport("hecdss")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetUnits(IntPtr ts);
    [DllImport("hecdss")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetType(IntPtr ts);
    [DllImport("hecdss")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetProgramName(IntPtr ts);
    [DllImport("hecdss")]
    private extern static IntPtr GetQuality(IntPtr ts);
    [DllImport("hecdss")]
    private extern static IntPtr GetDoubleValues(IntPtr ts);
    [DllImport("hecdss")]
    private extern static int GetTimeGranularitySeconds(IntPtr ts);
    [DllImport("hecdss")]
    private extern static void SetTimeGranularitySeconds(IntPtr ts, int value);
    [DllImport("hecdss")]
    private extern static int GetJulianBaseDate(IntPtr ts);
    [DllImport("hecdss")]
    private extern static IntPtr GetTimes(IntPtr ts);
    [DllImport("hecdss")]
    private extern static IntPtr GetDoubleProfileDepths(IntPtr ts);
    [DllImport("hecdss")]
    private extern static int GetProfileDepthsNumber(IntPtr ts);
    [DllImport("hecdss")]
    private extern static IntPtr GetDoubleProfileValues(IntPtr ts);
    [DllImport("hecdss")]
    private extern static int GetNumberValues(IntPtr ts);
    [DllImport("hecdss")]
    private extern static IntPtr GetLocation(IntPtr ts);
    [DllImport("hecdss")]
    private extern static int GetStartJulianDate(IntPtr ts);
    [DllImport("hecdss")]
    private extern static void SetStartJulianDate(IntPtr ts, int value);
    [DllImport("hecdss")]
    private extern static int GetStartTimeSeconds(IntPtr ts);
    [DllImport("hecdss")]
    private extern static void SetStartTimeSeconds(IntPtr ts, int value);
    [DllImport("hecdss")]
    private extern static IntPtr GetFloatValues(IntPtr ts);
    [DllImport("hecdss")]
    private extern static void SetFloatValues(IntPtr ts, float[] value, int arrayLength);
    [DllImport("hecdss")]
    private extern static int GetEndJulianDate(IntPtr ts);
    [DllImport("hecdss")]
    private extern static void SetEndJulianDate(IntPtr ts, int value);
    [DllImport("hecdss")]
    private extern static int GetEndTimeSeconds(IntPtr ts);
    [DllImport("hecdss")]
    private extern static void SetEndTimeSeconds(IntPtr ts, int value);
    [DllImport("hecdss")]
    private extern static int GetTimeIntervalSeconds(IntPtr ts);
    [DllImport("hecdss")]
    private extern static void SetTimeIntervalSeconds(IntPtr ts, int value);
    [DllImport("hecdss")]
    private extern static IntPtr GetCNotes(IntPtr ts);
    [DllImport("hecdss")]
    private extern static void SetCNotes(IntPtr ts, sbyte[] value, int arrayLength);
    [DllImport("hecdss")]
    private extern static int GetCNotesLengthTotal(IntPtr ts);
    [DllImport("hecdss")]
    private extern static void SetCNotesLengthTotal(IntPtr ts, int value);
    [DllImport("hecdss")]
    private extern static void SetQuality(IntPtr ts, int[] quality, int arraySize);
    [DllImport("hecdss")]
    private extern static void SetQualityElementSize(IntPtr ts, int elementSize);
    [DllImport("hecdss")]
    private extern static void SetDoubleProfileDepths(IntPtr ts, double[] values, int arrayLength);
    [DllImport("hecdss")]
    private extern static void SetProfileDepthsNumber(IntPtr ts, int value);
    [DllImport("hecdss")]
    private extern static void SetDoubleProfileValues(IntPtr ts, double[] values, int arrayLength);
    [DllImport("hecdss")]
    private extern static void SetNumberValues(IntPtr ts, int value);
    [DllImport("hecdss")]
    private extern static void SetUnitsProfileValues(IntPtr ts, string value);
    [DllImport("hecdss")]
    private extern static void SetUnitsProfileDepths(IntPtr ts, string value);
    [DllImport("hecdss")]
    private extern static void SetFloatProfileDepths(IntPtr ts, float[] values, int arrayLength);
    [DllImport("hecdss")]
    private extern static void SetFloatProfileValues(IntPtr ts, float[] values, int arrayLength);
    [DllImport("hecdss")]
    private extern static int GetCNotesSize(IntPtr ts);
    [DllImport("hecdss")]
    private extern static int SetCNotesSize(IntPtr ts, int value);


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
    public int TimeGranularitySeconds
    {
      get
      {
        return GetTimeGranularitySeconds(TheStruct);
      }
      set
      {
        SetTimeGranularitySeconds(TheStruct, value);
      }
    }
    public int JulianBaseDate
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

    public int StartJulianDate
    {
      get
      {
        return GetStartJulianDate(TheStruct);
      }
      set
      {
        SetStartJulianDate(TheStruct, value);
      }
    }
    public int StartTimeSeconds
    {
      get
      {
        return GetStartTimeSeconds(TheStruct);
      }
      set
      {
        SetStartTimeSeconds(TheStruct, value);
      }
    }
    public float[] FloatValues
    {
      get
      {
        IntPtr ptr = GetFloatValues(TheStruct);
        if (ptr != IntPtr.Zero)
        {
          float[] r = new float[NumberValues];
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
        SetFloatValues(TheStruct, value, value.Length);
      }
    }
    public int EndJulianDate
    {
      get
      {
        return GetEndJulianDate(TheStruct);
      }
      set
      {
        SetEndJulianDate(TheStruct, value);
      }
    }
    public int EndTimeSeconds
    {
      get
      {
        return GetEndTimeSeconds(TheStruct);
      }
      set
      {
        SetEndTimeSeconds(TheStruct, value);
      }
    }
    public int TimeIntervalSeconds
    {
      get
      {
        return GetTimeIntervalSeconds(TheStruct);
      }
      set
      {
        SetTimeIntervalSeconds(TheStruct, value);
      }
    }
    public int CNotesSize
    {
      get
      {
        return GetCNotesSize(TheStruct);
      }
      set
      {
        SetCNotesSize(TheStruct, value);
      }
    }
    public sbyte[] CNotes
    {
      get
      {
        IntPtr ptr = GetCNotes(TheStruct);
        if (ptr != IntPtr.Zero)
        {
          byte[] b = new byte[CNotesSize];
          Marshal.Copy(ptr, b, 0, b.Length);
          sbyte[] r = new sbyte[b.Length];
          for (int i = 0; i < b.Length; i++)
          {
            r[i] = (sbyte)b[i];
          }
          return r;
        }
        return null;
      }
      set
      {
        SetCNotes(TheStruct, value, value.Length);
      }
    }
    public int CNotesLengthTotal
    {
      get
      {
        return GetCNotesLengthTotal(TheStruct);
      }
      set
      {
        SetCNotesLengthTotal(TheStruct, value);
      }
    }
  }
}
