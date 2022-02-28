using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Hec.Dss.Native;
using static Hec.Dss.DssReader;

namespace Hec.Dss
{
  public static class DssGlobals
  {
    public static int SetProgramName(String programName)
    {
      return DSS.ZSet("PROG", programName, 0);
    }
    public static void SetMessageLevel(MethodID messageMethod = MethodID.MESS_METHOD_GENERAL_ID, 
      LevelID messageLevel = LevelID.MESS_LEVEL_GENERAL)
    {
      //Call the version 6 and 7 set message level first, if the file is 6 then the ZSetMessageLevel will not work, and we can still use MethodIDs for 7.
      DSS.ZSet("mlvl", "", (int)messageLevel);
      DSS.ZSetMessageLevel((int)messageMethod, (int)messageLevel);

    }

    public static void SetDefaultVersion(int version)
    {
      DSS.ZSet("DSSV", "", version);
    }
      
  }
}
