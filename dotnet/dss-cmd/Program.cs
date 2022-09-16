// See https://aka.ms/new-console-template for more information
using System.Runtime.InteropServices;


// Notes:
/*https://docs.microsoft.com/en-us/dotnet/framework/interop/copying-and-pinning
 * Pinning is automatically performed during marshalling for objects such as String
 * the interop marshaller passes an array as In parameters by default.
 * 
 * Strategy:  
 *     using string in signature for passing strings in (const char*)
 *     
 */

[DllImport("hecdss.dll")]
static extern IntPtr hec_dss_open(string fileName);

[DllImport("hecdss.dll")]
static extern int hec_dss_close(IntPtr dss_file);

[DllImport("hecdss.dll")]//, CharSet = CharSet.Auto)]
static extern int hec_dss_tsRetrieve(IntPtr pdss, string pathname, 
                                  string startDateTime,string endDateTime,
                                  int[] timeArray, double[] valueArray, int arraySize,
                                  ref int numberValuesRead, ref int julianBaseDate,
                                  ref char[] units, ref char[] type);

//////////////////////////////////////////////////////////////

string filename = "sample7.dss";
var dss = hec_dss_open(filename);

int buff_size = 5000;
string path = "//SACRAMENTO/PRECIP-INC/01Jan1877/1Day/OBS/";
int[] times = new int[buff_size];
double[] value = new double[buff_size];
int numberValuesRead = 0;
int julianBaseDate = 0;
char[] units = new char[30];
char[] type = new char[30];

hec_dss_tsRetrieve(dss, path, "", "", times, value, buff_size, ref numberValuesRead, ref julianBaseDate,
   ref units, ref type);
Console.WriteLine("numberValuesRead: "+numberValuesRead);

hec_dss_close(dss);