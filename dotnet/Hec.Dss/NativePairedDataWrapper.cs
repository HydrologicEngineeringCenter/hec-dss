using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace Hec.Dss
{
  public class NativePairedDataWrapper
  {
    [DllImport("hecdss")]
    private extern static IntPtr GetDoubleOrdinates(IntPtr pd); 
    [DllImport("hecdss")]
    private extern static IntPtr GetPdFloatValues(IntPtr pd);
    [DllImport("hecdss")]
    private extern static int GetNumberCurves(IntPtr pd);
    [DllImport("hecdss")]
    private extern static int GetNumberOrdinates(IntPtr pd);
    [DllImport("hecdss")]
    private extern static IntPtr GetPdDoubleValues(IntPtr pd);
    [DllImport("hecdss")]
    private extern static IntPtr GetLabels(IntPtr pd);
    [DllImport("hecdss")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetTypeDependent(IntPtr pd);
    [DllImport("hecdss")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetTypeIndependent(IntPtr pd);
    [DllImport("hecdss")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetUnitsDependent(IntPtr pd);
    [DllImport("hecdss")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetUnitsIndependent(IntPtr pd);
    [DllImport("hecdss")]
    private extern static IntPtr GetPdLocation(IntPtr pd);
    [DllImport("hecdss")]
    private extern static int GetPdNumberValues(IntPtr pd);
    [DllImport("hecdss")]
    private extern static int GetLabelsLength(IntPtr pd);

    public IntPtr TheStruct { get; set; }
    public double[] DoubleOrdinates
    {
      get
      {
        IntPtr ptr = GetDoubleOrdinates(TheStruct);
        if (ptr != IntPtr.Zero)
        {
          double[] r = new double[NumberOrdinates];
          Marshal.Copy(ptr, r, 0, r.Length);
          return r;
        }
        return null;
      }
    }
    public float[] FloatValues
    {
      get
      {
        IntPtr ptr = GetPdFloatValues(TheStruct);
        if (ptr != IntPtr.Zero)
        {
          float[] r = new float[NumberValues];
          Marshal.Copy(ptr, r, 0, r.Length);
          return r;
        }
        return null;
      }
    }
    public int NumberCurves
    {
      get
      {
        return GetNumberCurves(TheStruct);
      }
    }
    public int NumberOrdinates
    {
      get
      {
        return GetNumberOrdinates(TheStruct);
      }
    }
    public double[] DoubleValues
    {
      get
      {
        IntPtr ptr = GetPdDoubleValues(TheStruct);
        if (ptr != IntPtr.Zero)
        {
          double[] r = new double[NumberValues];
          Marshal.Copy(ptr, r, 0, r.Length);
          return r;
        }
        return null;
      }
    }
    public sbyte[] Labels
    {
      get
      {
        IntPtr ptr = GetLabels(TheStruct);
        if (ptr != IntPtr.Zero)
        {
          byte[] b = new byte[GetLabelsLength(TheStruct)];
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
    }
    public string TypeDependent
    {
      get
      {
        return GetTypeDependent(TheStruct);
      }
    }
    public string TypeIndependent
    {
      get
      {
        return GetTypeIndependent(TheStruct);
      }
    }
    public string UnitsDependent
    {
      get
      {
        return GetUnitsDependent(TheStruct);
      }
    }
    public string UnitsIndependent
    {
      get
      {
        return GetUnitsIndependent(TheStruct);
      }
    }
    public NativeLocationWrapper LocationStruct
    {
      get
      {
        NativeLocationWrapper loc = new NativeLocationWrapper();
        loc.TheStruct = GetPdLocation(TheStruct);
        return loc;
      }
    }

    public int NumberValues
    {
      get
      {
        return GetPdNumberValues(TheStruct);
      }
    }
  }
}
