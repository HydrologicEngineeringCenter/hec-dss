using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Hec.Dss.Native;

namespace Hec.Dss
{
 
  public class Grid
  {
    public string PathName { get; set; }

    public GridType GridType { get; set; }

    public string DataUnits { get; set; }

    /// <summary>
    /// DSS data type (statistic type for the data)
    /// PER_AVER = 0, PER_CUM = 1, INST_VAL = 2, INST_CUM = 3, FREQ = 4, INVAL = 5
    /// </summary>
    public DssDataType DataType { get; set; }

    public int LowerLeftCellX { get; set; }

    public int LowerLeftCellY { get; set; }

    public int NumberOfCellsX { get; set; }

    public int NumberOfCellsY { get; set; }

    public float CellSize { get; set; }

    public string SRSName { get; set; }

    public int SRSDefinitionType { get; set; }

    public string SRSDefinition { get; set; }

    public float XCoordOfGridCellZero { get; set; }

    public float YCoordOfGridCellZero { get; set; }

    private int StructVersion { get; set; } 
    private int Version { get; set; } 
    private int CompressionMethod { get; set; } 
    private int SizeOfCompressedElements { get; set; } 

    public string Info()
    {
      StringBuilder sb = new StringBuilder();

      sb.AppendLine("******** Printing GRID STRUCT ********");
      sb.AppendLine("GridStruct Version :" + StructVersion);
      sb.AppendLine("Path :" + PathName);
      sb.AppendLine("GridType :" + GridType);
      sb.AppendLine("Version : " + Version);
      sb.AppendLine("Data Units : " + DataUnits);
      sb.AppendLine("Data Type : " + DataType);
      //sb.AppendLine("Data Source : " + StorageDataType);
      sb.AppendLine("LowerLeftCellX : " + LowerLeftCellX);
      sb.AppendLine("LowerLeftCellY : " + LowerLeftCellY);
      sb.AppendLine("NumberOfCellsX : " + NumberOfCellsX);
      sb.AppendLine("NumberOfCellsY : " + NumberOfCellsY);
      sb.AppendLine("CellSize : " + CellSize.ToString("0.00000"));
      sb.AppendLine("CompressionMethod : " + CompressionMethod);
      sb.AppendLine("SizeofCompressedElements : " + SizeOfCompressedElements);

      sb.AppendLine("NumberOfRanges : " + NumberOfRanges);
      sb.AppendLine("SrsName : " + SRSName);
      sb.AppendLine("SrsDefinitionType : " + SRSDefinitionType);
      sb.AppendLine("_srsDefinition : " + SRSDefinition);
      sb.AppendLine("XCoordOfGridCellZero : " + XCoordOfGridCellZero.ToString("0.00000"));
      sb.AppendLine("YCoordOfGridCellZero : " + YCoordOfGridCellZero.ToString("0.00000"));
      sb.AppendLine("NullValue : " + NullValue.ToString("0.00000"));
      sb.AppendLine("TimeZoneID : " + TimeZoneID);
      sb.AppendLine("timeZoneRawOffset : " + TimeZoneRawOffset);
      sb.AppendLine("IsInterval : " + IsInterval);
      sb.AppendLine("IsTimeStamped : " + IsTimeStamped);
      sb.AppendLine("Storage Data Type  :" + StorageDataType);

      if (StorageDataType == 0) // GRID_FLOAT
      {
        sb.AppendLine("Max Data Value : " + MaxDataValue.ToString("0.00000"));
        sb.AppendLine("Min Data Value : " + MinDataValue.ToString("0.00000"));
        sb.AppendLine("Mean Data Value : " + MeanDataValue.ToString("0.00000"));
        sb.AppendLine("======== Range Limit Table ===========");

        if (RangeLimitTable != null && RangeLimitTable.Length > 0)
        {
          sb.AppendLine("           Range        > or =    Incremental Count");
          int size = NumberOfRanges;
          for (int i = 0; i < size - 1; i++)
          {
            var v = RangeLimitTable[i];
            var vs = v.ToString("0.00000");
            if (v == UndefinedValue)
              vs = "undefined".PadRight(16);
            sb.AppendLine(vs + " "
                + NumberEqualOrExceedingRangeLimit[i].ToString("0.00000"));
          }

          sb.AppendLine("====================================================");
        }
      }
      return sb.ToString();
    }

    public float NullValue { get; set; }

    public float UndefinedValue { get; set; }

    public string TimeZoneID { get; set; }

    public int TimeZoneRawOffset { get; set; }

    public bool IsInterval { get; set; }

    public bool IsTimeStamped { get; set; }

    public int NumberOfRanges { get; set; }

    private int StorageDataType { get; set; }  

    public float MinDataValue { get; set; } 

    public float MaxDataValue { get; set; } 

    public float MeanDataValue { get; set; }  

    public float[] RangeLimitTable { get; set; }  

    public int[] NumberEqualOrExceedingRangeLimit { get; set; } 

    public float[] Data { get; set; } 


    private string ConvertMilitaryTimeTo12Hour(string timeString)
    {
      string part1 = timeString.Substring(0, 2);
      string AMOrPM = "AM";
      int p1;
      Int32.TryParse(part1, out p1);
      if (p1 > 12)
      {
        AMOrPM = "PM";
        p1 -= 12;
      }
      string part2 = timeString.Substring(2);
      return p1 + ":" + part2 + AMOrPM;
    }
  }

}
