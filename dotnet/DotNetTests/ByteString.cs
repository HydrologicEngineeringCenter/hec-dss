using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DSSUnitTests
{
  /// <summary>
  /// Used for mashaling ASCII strings in/out of Native Code
  /// </summary>
  internal class ByteString:IDisposable
  {
    public ByteString(int size)
    {
      Data = new byte[size];
    }
    public ByteString(string s)
    {
      Data = Encoding.ASCII.GetBytes(s);
    }
    
    public byte[] Data { get; }

    public void Dispose()
    {
    }

    public override string ToString()
    {
      if (Data == null)
        return "";
      if (Data.Length == 0)
        return "";
      var end = System.Array.IndexOf(Data, (byte)0); // get rid of trailing \0
      int count = Math.Min(end, Data.Length);

      return Encoding.ASCII.GetString(Data,0,count);  
    }
 
  }
}
