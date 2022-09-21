// See https://aka.ms/new-console-template for more information
using System.Buffers;
using System.Runtime.InteropServices;
using System.Text;

[DllImport("hecdss.dll")]
static extern IntPtr hec_dss_open(string fileName);

[DllImport("hecdss.dll")]
static extern int hec_dss_close(IntPtr dss_file);

[DllImport("hecdss.dll", CharSet= CharSet.Ansi, ExactSpelling = true)]
static extern int hec_dss_tsRetrieve(IntPtr pdss, string pathname,
                                  string startDate, string startTime,
                                  string endDate,   string endTime,
                                  int[] timeArray, double[] valueArray, int arraySize,
                                  ref int numberValuesRead, ref int julianBaseDate,
                                  byte[] units,int unitsLength, byte[] type, int typeLength);


//dss_cmd.BasicPinvokeTests.Run();
//return;


string filename = "sample7.dss";
var dss = hec_dss_open(filename);

int buff_size = 5000;
string path = "//SACRAMENTO/PRECIP-INC/01Jan1877/1Day/OBS/";
int[] times = new int[buff_size];
double[] value = new double[buff_size];
int numberValuesRead = 0;
int julianBaseDate = 0;

var units = ArrayPool<byte>.Shared.Rent(32);
var type = ArrayPool<byte>.Shared.Rent(32);

string startDateTime = "01Jan1877 01:00";
string endDateTime = "31Jan1877 24:00";

int status = hec_dss_tsRetrieve(dss, path, startDateTime.Split(' ')[0], startDateTime.Split(' ')[1],
   endDateTime.Split(' ')[0], startDateTime.Split(' ')[1], times, value, buff_size, 
   ref numberValuesRead, ref julianBaseDate,
   units,units.Length, type, type.Length);

Console.WriteLine("numberValuesRead: "+numberValuesRead);
Console.WriteLine("units: " + Encoding.ASCII.GetString(units));
Console.WriteLine("type: " + Encoding.ASCII.GetString(type));

ArrayPool<byte>.Shared.Return(units);
ArrayPool<byte>.Shared.Return(type);


hec_dss_close(dss);