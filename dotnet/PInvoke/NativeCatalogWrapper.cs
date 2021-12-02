using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace Hec.Dss
{
  public class NativeCatalogWrapper
  {
    [DllImport("hecdss")]
    [return: MarshalAs(UnmanagedType.BStr)]
    private extern static string GetCatalogPathName(IntPtr cat, int pathNameIndex);
    [DllImport("hecdss")]
    private extern static int GetPathNameLength(IntPtr cat, int pathNameIndex);
    [DllImport("hecdss")]
    private extern static int GetNumberPathNames(IntPtr cat);
    [DllImport("hecdss")]
    private extern static IntPtr GetRecordType(IntPtr cat);
    [DllImport("hecdss")]
    private extern static int GetTypeWantedStart(IntPtr cat);
    [DllImport("hecdss")]
    private extern static void SetTypeWantedStart(IntPtr cat, int value);
    [DllImport("hecdss")]
    private extern static int GetTypeWantedEnd(IntPtr cat);
    [DllImport("hecdss")]
    private extern static void SetTypeWantedEnd(IntPtr cat, int value);

    public IntPtr TheStruct;

    public string[] PathNameList
    {
      get
      {
        int numberPaths = GetNumberPathNames(TheStruct);
        StringBuilder[] arr = new StringBuilder[numberPaths];
        string[] returnArray = new string[numberPaths];
        for (int i = 0; i < numberPaths; i++)
        {
          arr[i] = new StringBuilder(GetPathNameLength(TheStruct, i));
          arr[i].Append(GetCatalogPathName(TheStruct, i));
          returnArray[i] = arr[i].ToString();
        }

        return returnArray;
      }
    }
    public int[] RecordType
    {
      get
      {
        int numberPaths = GetNumberPathNames(TheStruct);
        IntPtr ptr = GetRecordType(TheStruct);
        if (ptr != IntPtr.Zero)
        {
          int[] returnArray = new int[GetNumberPathNames(TheStruct)];
          Marshal.Copy(ptr, returnArray, 0, returnArray.Length);
          return returnArray;
        }

        return null;
      }
    }

    public int TypeWantedStart
    {
      get
      {
        return GetTypeWantedStart(TheStruct);
      }
      set
      {
        SetTypeWantedStart(TheStruct, value);
      }
    }
    public int TypeWantedEnd
    {
      get
      {
        return GetTypeWantedEnd(TheStruct);
      }
      set
      {
        SetTypeWantedEnd(TheStruct, value);
      }
    }
  }
}
