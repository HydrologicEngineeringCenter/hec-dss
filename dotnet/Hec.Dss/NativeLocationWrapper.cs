using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace Hec.Dss
{
  public class NativeLocationWrapper
  {
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static double GetXOrdinate(IntPtr loc);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static double GetYOrdinate(IntPtr loc);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static double GetZOrdinate(IntPtr loc);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetCoordinateSystem(IntPtr loc);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetCoordinateID(IntPtr loc);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetHorizontalUnits(IntPtr loc);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetHorizontalDatum(IntPtr loc);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetVerticalUnits(IntPtr loc);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static int GetVerticalDatum(IntPtr loc);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetTimeZoneName(IntPtr loc);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetSupplemental(IntPtr loc);


    public IntPtr TheStruct;

    public double XOrdinate
    {
      get
      {
        return GetXOrdinate(TheStruct);
      }
    }
    public double YOrdinate
    {
      get
      {
        return GetYOrdinate(TheStruct);
      }
    }
    public double ZOrdinate
    {
      get
      {
        return GetZOrdinate(TheStruct);
      }
    }
    public int CoordinateSystem
    {
      get
      {
        return GetCoordinateSystem(TheStruct);
      }
    }
    public int CoordinateID
    {
      get
      {
        return GetCoordinateID(TheStruct);
      }
    }
    public int HorizontalUnits{
      get
      {
        return GetHorizontalUnits(TheStruct);
      }
    }
    public int HorizontalDatum
    {
      get
      {
        return GetHorizontalDatum(TheStruct);
      }
    }
    public int VerticalUnits{
      get
      {
        return GetVerticalUnits(TheStruct);
      }
    }
    public int VerticalDatum
    {
      get
      {
        return GetVerticalDatum(TheStruct);
      }
    }
    public string TimeZoneName
    {
      get
      {
        return GetTimeZoneName(TheStruct);
      }
    }
    public string Supplemental{
      get
      {
        return GetSupplemental(TheStruct);
      }
    }
  }
}
