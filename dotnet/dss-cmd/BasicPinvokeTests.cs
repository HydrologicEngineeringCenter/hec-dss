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

    // https://learn.microsoft.com/en-us/dotnet/framework/interop/marshalling-different-types-of-arrays
    /*
     * https://learn.microsoft.com/en-us/archive/msdn-magazine/2004/october/net-column-p-invoke-revisited#S2

    When the runtime marshaler sees that your code is passing to native code a reference to a managed reference object, it automatically pins the object. What this means is that the object is put in a special state where the garbage collector will neither move the object in memory nor remove the object from memory. Pinned objects hurt the performance of the garbage collector, but they assure that the memory remains intact for the life of the native call; this is critical to the proper functioning of the native code.

     *https://docs.microsoft.com/en-us/dotnet/framework/interop/copying-and-pinning
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
     * 
     * https://learn.microsoft.com/en-us/dotnet/framework/interop/marshalling-different-types-of-arrays
     * Unless an array is explicitly marshalled by reference, the default behavior marshals 
     * the array as an In parameter. You can change this behavior by applying the InAttribute 
     * and OutAttribute attributes explicitly.
     * 
     * 
     * 
     * https://stackoverflow.com/questions/2218444/when-passing-a-managed-byte-array-through-pinvoke-to-be-filled-in-by-win32-do
     * 
     * The CLR will automatically pin references to managed objects when they cross the 
     * PInvoke boundary. As soon as the PInvoke function exits the reference will be unpinned.
     * So in situations like having a native function fill a byte[] no manually pinning is necessary 
     * because the object is only used by native code during the function call.
     * 
     * 
     * 
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
