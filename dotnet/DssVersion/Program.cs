using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DssVersion
{
    /// <summary>
    /// prints DSS version information, can be saved to version_build.h
    /// </summary>
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length != 2)
            {
                Console.WriteLine("Usage: DssVersion hecdssInternal.h build");
                return;
            }

            UpdateTimeStampInHeader(args[0]);

            var v = GetDSSVersionString(args[0]);
            int major = Convert.ToInt32(v.Substring(0, 1));
            int idxDash = v.IndexOf("-");
            var minor = v.Substring(idxDash + 1, 1);
            var rev = v.Substring(idxDash + 2, 1);

            Console.WriteLine("#define VERSION_MAJOR " + major);
            Console.WriteLine("#define VERSION_MINOR " + getVersFromChar(minor[0]));
            Console.WriteLine("#define VERSION_REVISION " + getVersFromChar(rev[0]));
            Console.WriteLine("#define VERSION_BUILD " + args[1]);

        }

        /// <summary>
        /// Update the time stamp in hecdssInternal.h  
        /// #define DSS_VERSION_DATE "12 August 2019"
        /// </summary>
        /// <param name="v"></param>
        private static void UpdateTimeStampInHeader(string filename)
        {
            var lines = File.ReadAllLines(filename);
            var search = "#define DSS_VERSION_DATE";
            int idx = Array.FindIndex(lines, x => x.StartsWith(search));

            if (idx < 0)
                throw new Exception("DSS_VERSION_DATE not found");
            var t = DateTime.Now.Date;
            lines[idx] = "#define DSS_VERSION_DATE \"" + t.ToString("dd MMMM yyyy")+"\"";

            File.WriteAllLines(filename, lines);
        }

        /// <summary>
        /// reads  hecdssInternal.h
        /// to find the string  X-AB  
        /// #define DSS_VERSION "7-HL"
        /// </summary>
        /// <param name="filename"></param>
        /// <returns></returns>
        private static string GetDSSVersionString(string filename)
        {
            var lines = File.ReadAllLines(filename);
            var search = "#define DSS_VERSION";
            int idx = Array.FindIndex(lines, x => x.StartsWith(search));
            var s = lines[idx];
            s = s.Substring(search.Length).Trim().Trim('\"');

            return s;
        }

        static int getVersFromChar(char version)
        {
            //  Valid characters are A-Z, a-z
            //	A ==1, B ==2, a==27, b==28
            int iver = 0;
            if ((int)version < 65)
            {
                if (((int)version > 47) && ((int)version < 58))
                {
                    iver = (int)version - 48;
                    return iver;
                }
                else
                {
                    return 0;
                }
            }
            if ((int)version < 91)
            {
                iver = (int)version - 64;
            }
            if ((int)version > 91)
            {
                iver = (int)version - 96 + 26;
            }
            return iver;
        }


    }
}
