// See https://aka.ms/new-console-template for more information
using System.Buffers;
using System.Runtime.InteropServices;
using System.Text;



//dss_cmd.BasicPinvokeTests.Run();
//return;

IntPtr dss=(IntPtr)0;
string filename = "sample7.dss";
var status = DssNative.hec_dss_open(filename,out dss);

Console.WriteLine("version: " + DssNative.hec_dss_version(dss));

string path = "/GREEN RIVER/GLENFIR/FLOW/01Apr1992/1Hour/OBS/";
int numberValuesRead = 0;
int julianBaseDate = 0, timeGranularitySeconds = 0;

var units = ArrayPool<byte>.Shared.Rent(32);
var type = ArrayPool<byte>.Shared.Rent(32);

string startDateTime = "01Jan1877 01:00";
string endDateTime = "31Jan1877 24:00";
string startDate = "";
string startTime = "";
string endDate = ""; //endDateTime.Split(' ')[0]
string endTime = "";

int numberValues = 0;

DssNative.hec_dss_tsGetSizes(dss, path, startDate,startTime,endDate, endTime, ref numberValues);
Console.WriteLine(numberValues);

int[] times = new int[numberValues];
double[] value = new double[numberValues];

status = DssNative.hec_dss_tsRetrieve(dss, path, startDate, startTime, endDate, endTime,
    times, value, numberValues, 
   ref numberValuesRead, ref julianBaseDate,ref timeGranularitySeconds,
   units,units.Length, type, type.Length);

Console.WriteLine("numberValuesRead: "+numberValuesRead);
Console.WriteLine("units: " + Encoding.ASCII.GetString(units));
Console.WriteLine("type: " + Encoding.ASCII.GetString(type));

ArrayPool<byte>.Shared.Return(units);
ArrayPool<byte>.Shared.Return(type);


DssNative.hec_dss_close(dss);