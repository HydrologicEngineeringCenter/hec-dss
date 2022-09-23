// See https://aka.ms/new-console-template for more information
using System.Buffers;
using System.Runtime.InteropServices;
using System.Text;



//dss_cmd.BasicPinvokeTests.Run();
//return;


string filename = "sample7.dss";
var dss = DssNative.hec_dss_open(filename);

int buff_size = 5000;
string path = "/GREEN RIVER/GLENFIR/FLOW/01Apr1992/1Hour/OBS/";
int[] times = new int[buff_size];
double[] value = new double[buff_size];
int numberValuesRead = 0;
int julianBaseDate = 0;

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

int status = DssNative.hec_dss_tsRetrieve(dss, path, startDate, startTime, endDate, endTime, times, value, buff_size, 
   ref numberValuesRead, ref julianBaseDate,
   units,units.Length, type, type.Length);

Console.WriteLine("numberValuesRead: "+numberValuesRead);
Console.WriteLine("units: " + Encoding.ASCII.GetString(units));
Console.WriteLine("type: " + Encoding.ASCII.GetString(type));

ArrayPool<byte>.Shared.Return(units);
ArrayPool<byte>.Shared.Return(type);


DssNative.hec_dss_close(dss);