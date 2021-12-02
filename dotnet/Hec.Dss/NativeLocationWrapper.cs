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
    [DllImport("hecdss")]
    private extern static double GetXOrdinate(IntPtr loc);
    [DllImport("hecdss")]
    private extern static double GetYOrdinate(IntPtr loc);
    [DllImport("hecdss")]
    private extern static double GetZOrdinate(IntPtr loc);
    [DllImport("hecdss")]
    private extern static int GetCoordinateSystem(IntPtr loc);
    [DllImport("hecdss")]
    private extern static int GetCoordinateID(IntPtr loc);
    [DllImport("hecdss")]
    private extern static int GetHorizontalUnits(IntPtr loc);
    [DllImport("hecdss")]
    private extern static int GetHorizontalDatum(IntPtr loc);
    [DllImport("hecdss")]
    private extern static int GetVerticalUnits(IntPtr loc);
    [DllImport("hecdss")]
    private extern static int GetVerticalDatum(IntPtr loc);
    [DllImport("hecdss")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetTimeZoneName(IntPtr loc);
    [DllImport("hecdss")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetSupplemental(IntPtr loc);

    [DllImport("hecdss")]
    private extern static void SetXOrdinate(IntPtr loc, double value);
    [DllImport("hecdss")]
    private extern static void SetYOrdinate(IntPtr loc, double value);
    [DllImport("hecdss")]
    private extern static void SetZOrdinate(IntPtr loc, double value);
    [DllImport("hecdss")]
    private extern static void SetCoordinateSystem(IntPtr loc, int value);
    [DllImport("hecdss")]
    private extern static void SetCoordinateID(IntPtr loc, int value);
    [DllImport("hecdss")]
    private extern static void SetHorizontalUnits(IntPtr loc, int value);
    [DllImport("hecdss")]
    private extern static void SetHorizontalDatum(IntPtr loc, int value);
    [DllImport("hecdss")]
    private extern static void SetVerticalUnits(IntPtr loc, int value);
    [DllImport("hecdss")]
    private extern static void SetVerticalDatum(IntPtr loc, int value);
    [DllImport("hecdss")]
    private extern static void SetTimeZoneName(IntPtr loc, string value);
    [DllImport("hecdss")]
    private extern static void SetSupplemental(IntPtr loc, string value);
    [DllImport("hecdss")]
    private extern static void SetPathName(IntPtr loc, string value);


    public IntPtr TheStruct;

    public double XOrdinate
    {
      get
      {
        return GetXOrdinate(TheStruct);
      }
      set
      {
        SetXOrdinate(TheStruct, value);
      }
    }
    public double YOrdinate
    {
      get
      {
        return GetYOrdinate(TheStruct);
      }
      set
      {
        SetYOrdinate(TheStruct, value);
      }
    }
    public double ZOrdinate
    {
      get
      {
        return GetZOrdinate(TheStruct);
      }
      set
      {
        SetZOrdinate(TheStruct, value);
      }
    }
    public int CoordinateSystem
    {
      get
      {
        return GetCoordinateSystem(TheStruct);
      }
      set
      {
        SetCoordinateSystem(TheStruct, value);
      }
    }
    public int CoordinateID
    {
      get
      {
        return GetCoordinateID(TheStruct);
      }
      set
      {
        SetCoordinateID(TheStruct, value);
      }
    }
    public int HorizontalUnits{
      get
      {
        return GetHorizontalUnits(TheStruct);
      }
      set
      {
        SetHorizontalUnits(TheStruct, value);
      }
    }
    public int HorizontalDatum
    {
      get
      {
        return GetHorizontalDatum(TheStruct);
      }
      set
      {
        SetHorizontalDatum(TheStruct, value);
      }
    }
    public int VerticalUnits{
      get
      {
        return GetVerticalUnits(TheStruct);
      }
      set
      {
        SetVerticalUnits(TheStruct, value);
      }
    }
    public int VerticalDatum
    {
      get
      {
        return GetVerticalDatum(TheStruct);
      }
      set
      {
        SetVerticalDatum(TheStruct, value);
      }
    }
    public string TimeZoneName
    {
      get
      {
        return GetTimeZoneName(TheStruct);
      }
      set
      {
        SetTimeZoneName(TheStruct, value);
      }
    }
    public string Supplemental{
      get
      {
        return GetSupplemental(TheStruct);
      }
      set
      {
        SetSupplemental(TheStruct, value);
      }
    }

    public string PathName
    {
      set
      {
        SetPathName(TheStruct, value);
      }
    }
  }
}
