// See https://aka.ms/new-console-template for more information
using System.Runtime.InteropServices;

Console.WriteLine("Hello, World!");

[DllImport("hecdss.dll")]
static extern IntPtr hec_dss_open(string fileName);

[DllImport("hecdss.dll")]
static extern int hec_dss_close(IntPtr dss_file);

[DllImport("hecdss.dll")]
static extern int hec_dss_tsRetrieve(IntPtr pdss, string pathname);

//////////////////////////////////////////////////////////////

string filename = "sample7.dss";
var dss = hec_dss_open(filename);

hec_dss_tsRetrieve(dss, "//SACRAMENTO/PRECIP-INC/01Jan1877/1Day/OBS/");


hec_dss_close(dss);