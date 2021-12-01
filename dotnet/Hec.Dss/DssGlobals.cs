using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Hec.Dss
{
  public static class DssGlobals
  {
    static public int SetProgramName(String programName)
    {
      return PInvoke.ZSet("PROG", programName, 0);
    }
  }
}
