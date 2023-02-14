using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Hec.Dss
{

  public enum RecordType { RegularTimeSeries, IrregularTimeSeries, RegularTimeSeriesProfile, PairedData, Grid, Tin, LocationInfo, Text, Unknown };
  /// <summary>
  /// DSS Data type (statistic)
  /// </summary>
  public enum DssDataType
  {
    PER_AVER = 0,
    PER_CUM = 1,
    INST_VAL = 2,
    INST_CUM = 3,
    FREQ = 4,
    INVAL = 5
  }

  /// <summary>
  /// DSS Record Type
  /// </summary>
  public enum GridType
  {
    UNDEFINED_GRID_TYPE = 400,
    HRAP = 410,
    ALBERS = 420, // SHG 
    SPECIFIED_GRID_TYPE = 430
  }

}
