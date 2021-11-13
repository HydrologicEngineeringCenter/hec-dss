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
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetQuality(IntPtr ts, int[] quality, int arraySize);
    [DllImport(@"..\..\..\PInvoke\x64\Debug\PInvoke")]
    private extern static void SetQualityElementSize(IntPtr ts, int elementSize);

    public IntPtr TheStruct;

    public int[] Quality 
    { 
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
  }
}
