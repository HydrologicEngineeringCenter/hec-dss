using System;
using System.IO;
using System.IO.Compression;
using System.Text;

namespace Hec.Dss
{
  class Compression
  {

    internal static string UnCompress(string str)
    {
      var b64 = Convert.FromBase64String(str);
      return UnCompress(b64);
    }

    private static string UnCompress(byte[] b64)
    {
      MemoryStream input = new MemoryStream(b64);
      MemoryStream output = new MemoryStream();
      using (GZipStream dstream = new GZipStream(input, CompressionMode.Decompress))
      {
        dstream.CopyTo(output);
      }
      var deflated = output.ToArray();
      var x = System.Text.Encoding.UTF8.GetString(deflated);
      return x;
    }



    private static void CopyTo(Stream src, Stream dest)
    {
      byte[] bytes = new byte[4096];

      int cnt;

      while ((cnt = src.Read(bytes, 0, bytes.Length)) != 0)
      {
        dest.Write(bytes, 0, cnt);
      }
    }

    internal static string Compress(string str)
    {
      var gzip = CompressInternal(str);
      return Convert.ToBase64String(gzip);
    }


    private static byte[] CompressInternal(string str)
    {
      var bytes = Encoding.UTF8.GetBytes(str);

      using (var msi = new MemoryStream(bytes))
      using (var mso = new MemoryStream())
      {
        using (var gs = new GZipStream(mso, CompressionMode.Compress))
        {
          //msi.CopyTo(gs);
          CopyTo(msi, gs);
        }

        return mso.ToArray();
      }
    }
    /*
    static string UnCompresszlib(byte[] b64)
    {
        int skipFirstBytes = 0;
        if (b64[0] == 0x78) // Default zlib header implementation... Can this change? How do we ID a generic zlib header?
            skipFirstBytes = 2;
        else if (b64[0] == 0x58 && b64[1] == 0x85) // See our WriteTile function - these are *our* 'zlib header' bytes.
            skipFirstBytes = 2;

        //skipFirstBytes = 2;
        MemoryStream input = new MemoryStream(b64, skipFirstBytes, b64.Length - skipFirstBytes);
        MemoryStream output = new MemoryStream();
        using (DeflateStream dstream = new DeflateStream(input, CompressionMode.Decompress))
        {
            dstream.CopyTo(output);
        }
        var deflated = output.ToArray();
        var x = System.Text.Encoding.UTF8.GetString(b64);
        return x;
    }
    */

  }
}
