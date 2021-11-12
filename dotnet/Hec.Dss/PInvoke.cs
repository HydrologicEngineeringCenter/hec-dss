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
    public extern static int ZTsStore(long[] ifltab, IntPtr tss, int storageFlag);
  }
}
