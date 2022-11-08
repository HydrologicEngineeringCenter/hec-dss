using System;
using System.Buffers;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace dss_cmd
{
   internal class BasicPinvokeTests
   {

      // Notes:
      /*https://docs.microsoft.com/en-us/dotnet/framework/interop/copying-and-pinning
       * Pinning is automatically performed during marshalling for objects such as String
       * the interop marshaller passes an array as In parameters by default.
       * 
       * Pinning is automatically performed during marshalling for objects such as String, 
       * however you can also manually pin memory using the GCHandle class.
       * 
       * 
       * https://learn.microsoft.com/en-us/dotnet/standard/native-interop/best-practices
       * 
       * using string to pass by value (const char* in Native)
       * 
       */




      static string GetWindowsDirectory()
      {
         [DllImport("kernel32.dll", CharSet = CharSet.Unicode)]
         static extern int GetWindowsDirectory(char[] buffer, int maxChars);

         var array = ArrayPool<char>.Shared.Rent(256);
         try
         {
            int length = GetWindowsDirectory(array, 256);
            return new string(array, 0, length).ToString();
         }
         finally { ArrayPool<char>.Shared.Return(array); }
      }

      public static void Run()
      {

         [DllImport("hecdss", ExactSpelling = true)]
         static extern int hec_dss_test_modify_arg(ref int value);

         [DllImport("hecdss", ExactSpelling = true)]
         static extern int hec_dss_test_string_const(string str, int size);

         [DllImport("hecdss", ExactSpelling = true)]
         static extern int hec_dss_test_string_buffer(StringBuilder outStr, int size);

         [DllImport("hecdss", ExactSpelling = true)]
         static extern int hec_dss_test_string_char(byte[] outStr, int size);

      [DllImport("hecdss", ExactSpelling = true)]
      static extern int hec_dss_test_list_of_string(byte[] s, int numberOfStrings, int size);

      int numberOfStrings = 10;
      int size = 400;
      var array2 = ArrayPool<byte>.Shared.Rent(numberOfStrings*size);
      hec_dss_test_list_of_string(array2, numberOfStrings,size);
      
      for (int i = 0; i < numberOfStrings; i++)
      {
        string a = Encoding.ASCII.GetString(array2,i*size,400);
        Console.WriteLine("C#:'" + a+"'");
      }
      
      ArrayPool<byte>.Shared.Return(array2);

      Console.WriteLine(GetWindowsDirectory());

         int x = 0;
         hec_dss_test_modify_arg(ref x);
         Console.WriteLine("x = "+x);

         string s = "hello";
         hec_dss_test_string_const(s, s.Length);

         StringBuilder sb = new StringBuilder(128);
         hec_dss_test_string_buffer(sb, sb.Capacity);
         Console.WriteLine("outStr = " + sb.ToString());
         
         
         byte[] array = ArrayPool<byte>.Shared.Rent(64);
         hec_dss_test_string_char(array, array.Length);
         s = Encoding.ASCII.GetString(array);
         Console.WriteLine("After call: s = " + s);
         ////Console.WriteLine("hec_dss_test_string_char: "+s2);
         ArrayPool<byte>.Shared.Return(array);

      }
   }
}
