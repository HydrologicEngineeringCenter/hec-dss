using Hec.Dss;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace nuget_ref_project
{
  class Program
  {
        static void Main(string[] args)
        {
            string fn = @"C:\Users\q0hecngn\Downloads\precip.2018.09.dss";
            string pathname = "/SHG/MARFC/PRECIP///NEXRAD/";
            var targetDSSPath = new DssPath(pathname);

            double sum = 0;
            for (int j = 0; j < 4; j++)
            {


                using (var dssr = new DssReader(fn, DssReader.MethodID.MESS_METHOD_GENERAL_ID, DssReader.LevelID.MESS_LEVEL_CRITICAL))
                {
                    var cat = dssr.GetCatalog(false);
                    var paths = cat.Paths.Select(dssPath => dssPath.FullPath).ToArray();

                    var filterPaths = PathAssist.FilterByPart(paths, targetDSSPath);

                    for (int i = 0; i < filterPaths.Count; i++)
                    {
                        string subpath = filterPaths[i];
                        var dssPath = new DssPath(subpath);

                        var grd = dssr.GetGrid(dssPath, true);
                        sum += grd.MaxDataValue;
                    }

                    Console.WriteLine(sum.ToString());
                }
            }
        }
  }
}
