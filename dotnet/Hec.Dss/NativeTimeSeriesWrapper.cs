using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Hec.Dss
{
  public class NativeTimeSeriesWrapper
  {
    public IntPtr TheStruct;

    public int[] Quality { get; internal set; }
    public int QualityArraySize { get; internal set; }
    public int QualityElementSize { get; internal set; }
  }
}
