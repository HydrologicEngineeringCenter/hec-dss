using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Hec.Dss.Native;
using Hec.Dss;

namespace DSSUnitTests
{
  [TestClass]
  public class DateTimeTesting
  {

    void ExtractYMD(string date, int expectedY, int expectedM, int expectedD)
    {
      Console.WriteLine(date);
      int year = 0, month = 0, day = 0;
      int status = PInvoke.DateToYearMonthDay(date, ref year, ref month, ref day);
      Console.WriteLine(year);
      Console.WriteLine(month);
      Console.WriteLine(day);
      Assert.AreEqual(0, status);
      Assert.AreEqual(expectedY, year);
      Assert.AreEqual(expectedM, month);
      Assert.AreEqual(expectedD, day);
    }

    void AssertExtractJulian(string date, int expectedJulian)
    {
      int j = PInvoke.DateToJulian(date);
      Assert.IsTrue(PInvoke.IsTimeDefined(j, 0));
      Assert.AreEqual(expectedJulian, j);
    }

    [TestMethod]
    public void ExtendedDateTest()
    {
      string[] dateStr = new string[]{
"01Jan-10000",
"09Jun1880",
"10Jun1881",
"20Jun1882",
"04Jun1886",
"27Jun1887",
"25Jun1888",
"30Jun1891",
"07Jul1892",
"19Jun1893",
"13Jun1894",
"23Jun1896",
"30Jun1898",
"23Apr1899",
"17Jun1900",
"21Jun1901",
"12Jun1902",
"10Jul1903",
"10Jun1904",
"07Jul1905",
"16Jun1906",
"18Jun1907",
"23Jun1908",
"15Jun1909",
"28Jun1910",
"30Jun1911",
"10Jul1912",
"07Jun1913",
"13Jun1914" };

      for (int i = 0; i < dateStr.Length; i++)
      {
        int j = PInvoke.DateToJulian(dateStr[i]);
        Console.WriteLine(dateStr[i] + ", " + j);
      }
      /*
      01Jan - 10000, 2400, value is 500000.000000
      09Jun1880, 2400, value is 123456.000000
      10Jun1881, 2400, value is 156700.000000
      20Jun1882, 2400, value is 144000.000000
      04Jun1886, 2400, value is 119700.000000
      27Jun1887, 2400, value is 168400.000000
      25Jun1888, 2400, value is 181000.000000
      30Jun1891, 2400, value is 133300.000000
      07Jul1892, 2400, value is 151800.000000
      19Jun1893, 2400, value is 121700.000000
      13Jun1894, 2400, value is 154700.000000
      23Jun1896, 2400, value is 124600.000000
      30Jun1898, 2400, value is 159900.000000
      23Apr1899, 2400, value is 137900.000000
      17Jun1900, 2400, value is 116900.000000
      21Jun1901, 2400, value is 144900.000000
      12Jun1902, 2400, value is 93600.000000
      10Jul1903, 2400, value is 106200.000000
      10Jun1904, 2400, value is 126900.000000
      07Jul1905, 2400, value is 191100.000000
      16Jun1906, 2400, value is 183800.000000
      18Jun1907, 2400, value is 148600.000000
      23Jun1908, 2400, value is 196000.000000
      15Jun1909, 2400, value is 199300.000000
      28Jun1910, 2400, value is 92000.000000
      30Jun1911, 2400, value is 117800.000000
      10Jul1912, 2400, value is 159900.000000
      07Jun1913, 2400, value is 124700.000000
      13Jun1914, 2400, value is 128800.000000
      */
    }

    public void BugInvalidDate()
    {
      int j = PInvoke.DateToJulian("22JUN2016 12:00");// this is NOT a valid date, because of the time.
      Console.WriteLine(j);
      Assert.IsFalse(PInvoke.IsTimeDefined(j, 0));// = -693786(NOT OK ?)
    }

    [TestMethod]
    public void StringConversions()
    {
      //DSS.DateToJulian("22JUN2016 12:00");
      ExtractYMD("28:Nov:2008", 2008, 11, 28);

      ExtractYMD("11/28/2008", 2008, 11, 28);
      ExtractYMD("November 28, 2008", 2008, 11, 28);
      ExtractYMD("22JUN2016", 2016, 6, 22);
      AssertExtractJulian("22JUN2016", 42542);

      ExtractYMD("01Jan-10000", -10000, 1, 1);

      ExtractYMD("01/01/-10000", -10000, 1, 1);

      Assert.IsFalse(PInvoke.IsTimeDefined(PInvoke.DateToJulian("22HEC2016"), 0));
    }

    [TestMethod]
    public void ConvertFromHECTests()
    {
      var t = new DateTime(1944, 6, 6, 6, 30, 0);
      Time.DateTimeToHecDateTime(t, out string date, out string time);

      Assert.AreEqual("06Jun1944", date);
      Assert.AreEqual("0630", time);

      t = new DateTime(2020, 2, 29, 12, 13, 14);
      Time.DateTimeToHecDateTime(t, out date, out time);

      Assert.AreEqual("29Feb2020", date);
      Assert.AreEqual("12:13:14", time);

      t = Time.ConvertFromHecDateTime("31Dec1964", "2400");

      Assert.AreEqual(1965, t.Year);
      Assert.AreEqual(1, t.Day);
      Assert.AreEqual(1, t.Month);


      t = Time.ConvertFromHecDateTime("15JAN2016:2400");
      Assert.AreEqual(2016, t.Year);
      Assert.AreEqual(16, t.Day);
      Assert.AreEqual(1, t.Month);
      Assert.AreEqual(0, t.Minute);
      Assert.AreEqual(0, t.Hour);

      t = Time.ConvertFromHecDateTime("15JAN2016 24:00:00");
      Assert.AreEqual(2016, t.Year);
      Assert.AreEqual(16, t.Day);
      Assert.AreEqual(1, t.Month);
      Assert.AreEqual(0, t.Minute);
      Assert.AreEqual(0, t.Hour);


      t = Time.ConvertFromHecDateTime("15JAN2016", "17:06:05");

      Assert.AreEqual(2016, t.Year);
      Assert.AreEqual(15, t.Day);
      Assert.AreEqual(1, t.Month);
      Assert.AreEqual(17, t.Hour);
      Assert.AreEqual(6, t.Minute);
      Assert.AreEqual(5, t.Second);

      t = Time.ConvertFromHecDateTime("15JAN2016", "1706");

      Assert.AreEqual(2016, t.Year);
      Assert.AreEqual(15, t.Day);
      Assert.AreEqual(1, t.Month);
      Assert.AreEqual(17, t.Hour);
      Assert.AreEqual(6, t.Minute);
      Assert.AreEqual(0, t.Second);

    

    }

    [TestMethod]
    public void TestTryConvertFromHecDateTime()
    {
      DateTime dt;
      var t = Time.TryConvertFromHecDateTime("21Jun1972 03:00:00.000", out dt);
      Assert.IsFalse(t);
    }

  }
}
