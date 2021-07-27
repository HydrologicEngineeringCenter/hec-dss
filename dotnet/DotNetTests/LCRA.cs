using System;
using Hec.Dss;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace DSSUnitTests
{
  [TestClass]
  public class LCRAO
  {
    [TestMethod]
    public void Hydromet()
    {
      DateTime t1 = new DateTime(2018, 7, 14);
      DateTime t2 = new DateTime(2019, 7, 14);
      string[] path = new string[] { };
      ReadSQLWriteDSSRegularTS_Type("lcra_hydromet.dss", "proc1", "PC", t1.ToString(), t2.ToString(), path, "cs");
      ReadSQLWriteDSSRegularTS_Type("lcra_hydromet.dss", "proc1", "FLOW", t1.ToString(), t2.ToString(), path, "cs");
      ReadSQLWriteDSSRegularTS_Type("lcra_hydromet.dss", "proc1", "ELEV", t1.ToString(), t2.ToString(), path, "cs");
      ReadSQLWriteDSSRegularTS_Type("lcra_hydromet.dss", "proc1", "STAG", t1.ToString(), t2.ToString(), path, "cs");
    }


    private static LocationInformation CreateLocation(string tzName)
    {
      LocationInformation loc = new LocationInformation();
      loc.TimeZoneName = tzName;
      loc.HorizontalUnits = 1; // feet
      loc.VerticalDatum = 1; // nav88
      loc.Supplemental = "Supplemental";
      return loc;
    }

    public static void ReadSQLWriteDSSRegularTS_Type(
        string DSSFileName,
        string ProcName,
        string TypeStr,
        string StartDate,
        string EndDate,
        string[] PathStrArr,
        string ConnectStr)
    {

      // ++++ int entriesPerRecord = ((int)(Convert.ToDateTime(EndDate) - Convert.ToDateTime(StartDate)).TotalHours) + 1;
      int entriesPerRecord = ((int)(Convert.ToDateTime(EndDate) - Convert.ToDateTime(StartDate)).TotalHours) + 2;

      //UTC dateValue.AddHours(hour));

      if (entriesPerRecord <= 0) entriesPerRecord = 0;
      // Console.Write("\nEntries per sensor: " + Convert.ToString(entriesPerRecord) + "");

      //test DSSDelete(DSSFileName);

      // DSSPath will look like "//1090_MWCT2/PRECIP/01FEB2000/1HOUR/HYDROMET/"

      // Query returns Path B/C: SiteName/SensorUnits

      //;dataUnits=ft;dataType=INST-VAL
      //;dataUnits=cfs;dataType=INST-VAL

      // Metadata
      // CoordinateSystems cs = CoordinateSystems.LatLong;
      //DatumUnits du = DatumUnits.DecimalDegrees;
      //HorizontalDatums hd = HorizontalDatums.Other;
      TimeSpan tzOffset = new TimeSpan(0, 0, 0); //14 minutes past PST
      string tzName = "CST"; // "UTC", "PST"
      string dataType = "INST-VAL"; // "INST-CUM"
      string dataUnits = "FT";
      string dssPathPart1 = "//";
      string dssPathPart2 = "/CFS/01FEB2000/1HOUR/HYDROMET/";
      //string supp = "Hydromet Data\0";
      //int cid = 1;

      if (TypeStr.Substring(0, 2).ToUpper() == "PC")
      {
        dataType = "INST-CUM";
        dataUnits = "IN";
        dssPathPart1 = "//";
        dssPathPart2 = "/PRECIP/01FEB2000/1HOUR/HYDROMET/";
      }
      else if (TypeStr.Substring(0, 4).ToUpper() == "FLOW")
      {
        dataType = "INST-VAL";
        dataUnits = "CFS";
        dssPathPart1 = "//";
        dssPathPart2 = "/CFS/01FEB2000/1HOUR/HYDROMET/";
      }
      else if (TypeStr.Substring(0, 4).ToUpper() == "ELEV")
      {
        dataType = "INST-VAL";
        dataUnits = "FT";
        dssPathPart1 = "//";
        dssPathPart2 = "/ELEV/01FEB2000/1HOUR/HYDROMET/";
      }
      else if (TypeStr.Substring(0, 4).ToUpper() == "STAG")
      {
        dataType = "INST-VAL";
        dataUnits = "FT";
        dssPathPart1 = "//";
        dssPathPart2 = "/STAGE/01FEB2000/1HOUR/HYDROMET/";
      }
      else
      {
        dssPathPart1 = "NULL";
        dssPathPart2 = "NULL";
      }
      // Entries - DSS data timestamps are inferred from the firstEntry and the "1HOUR" tag in the DSSPath
      DateTime firstEntry = new DateTime(2000, 2, 1, 12, 0, 0);

      //++++++++++++++ change from double to float
      //double[] values = new double[entriesPerRecord];
      //double[] valuesReturned = new double[entriesPerRecord];

      double[] values = new double[entriesPerRecord];
      double[] valuesReturned = new double[entriesPerRecord];

      //++++++++++++++ changed from double to float

      for (int j = 0; j < values.Length; j++)
        values[j] = -901;

      int sensor_number = -1;
      int CurrentSensor = 0;
      int valCount = 0;
      int idxhours;
      DateTime date_time = DateTime.Now.Date;
      string dssPath = "-1";
      string nws_id = "";

      using (DssWriter dw = new DssWriter(DSSFileName))
      {
        LCRAFakeData dr = new LCRAFakeData();
        CurrentSensor = 0;
        valCount = 0;
        while (dr.Read())
        {
          sensor_number = Convert.ToInt32(dr[0]);
          /* -----------------------------------------------------------------------
              if (readMeta)
              {
                  int index = Array.IndexOf(senNumb, sensor_number);
                  dataUnits = 
                  dataType = 
              }
                  ----------------------------------------------------------------------- */
          if (CurrentSensor != sensor_number)
          {
            if (valCount > 0)
            {
              TimeSeries ts = new TimeSeries();
              ts.Path = new DssPath(dssPath);
              ts.Values = values;
              ts.Units = dataUnits;
              ts.DataType = dataType;
              ts.StartDateTime = firstEntry;
              dw.Write(ts);
              //dw.StoreTimeSeriesRegular(dssPath, values, 0, firstEntry, dataUnits, dataType);
              LocationInformation loc = CreateLocation(tzName);
              dw.StoreLocation(dssPath, loc);
              //dw.StoreTimeSeriesRegular(
              //dssPath,
              //firstEntry,
              //values,
              //dataUnits,
              //dataType,
              //1, //(double)(valCount + .2),
              //1, //(double)(valCount),
              //supp,
              //tzName,
              //tzOffset,
              //cs,
              //hd,
              //du,
              //cid);

              for (int j = 0; j < values.Length; j++)
                values[j] = -901;
              valCount = 0;
            }
            CurrentSensor = sensor_number;
            nws_id = Convert.ToString(dr[1]);
            dssPath = dssPathPart1 + CurrentSensor.ToString() + "_" + nws_id + dssPathPart2;
            firstEntry = Convert.ToDateTime(dr[2]);
          }

          date_time = Convert.ToDateTime(dr[2]);
          idxhours = (int)(date_time - firstEntry).TotalHours;
          if (idxhours > entriesPerRecord)
          {
            Console.Error.WriteLine("Dataindex " + Convert.ToString(idxhours) +
                " returned exceeds " + Convert.ToString(entriesPerRecord) + " records");
            //ierror = 1;
          }
          else if (idxhours < 0)
          {
            Console.Error.WriteLine("Data returned prior to start date");
            //ierror = 2;
          }
          else
          {
            values[idxhours] = Convert.ToSingle(dr[3]); // Accum Rain (Ignore incremental rain column dr[4]) //xxx.ToDouble(dr[3]); 
            valCount = valCount + 1;
            //debug Console.WriteLine(Convert.ToString(idxhours) + ", " + Convert.ToString(CurrentSensor) +
            //debug  ", " + Convert.ToString(date_time) + ", " + Convert.ToString(values[idxhours]));
          }
          //values2[ii] = Convert.ToSingle(dr[3]); //xxx.ToDouble(dr[3]);
          //items[i] = Convert.ToString(dr[4]);
        }
        //} while (dr.NextResult());

        // closed & disposed via 'using' context, but done before dss write to limit usage
        dr.Close();

        if (valCount > 0)
        {
          //debug Console.WriteLine(Convert.ToString(valCount) + " Final Records Output, Sensor: " + 
          //debug  Convert.ToString(CurrentSensor) + "Date: " + Convert.ToString(DateTime.Now.Hour));

          TimeSeries ts =new TimeSeries(dssPath, values, firstEntry, dataUnits, dataType);
          dw.Write(ts);
          LocationInformation loc = CreateLocation(tzName);
          dw.StoreLocation(dssPath, loc);
          //dw.WriteTimeSeriesRegularComplete(
          //    dssPath,
          //    firstEntry,
          //    values,
          //    dataUnits,
          //    dataType,
          //    1, //(double)(valCount + .2),
          //    1, //(double)(valCount),
          //    supp,
          //    tzName,
          //    tzOffset,
          //    cs,
          //    hd,
          //    du,
          //    cid);
        }




      }
    }


  }
}
