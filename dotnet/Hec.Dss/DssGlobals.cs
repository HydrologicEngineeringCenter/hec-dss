using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static Hec.Dss.DssReader;

namespace Hec.Dss
{
  public static class DssGlobals
  {
    public static int SetProgramName(String programName)
    {
      return DssNative.hec_dss_set_string("PROG", programName);
    }
    public static void SetMessageLevel(int level)
    {
      DssNative.hec_dss_set_value("mlvl", level);
    }
      
  }
}
