using System;

namespace DSSUnitTests
{
  internal class LCRAFakeData
  {
    int counter;
    float precip = 0;
    public LCRAFakeData()
    {
      counter = 0;
      precip = 0;
    }

    public object this[int i]
    {
      get
      {
        if (i == 0) // sensor number
          return counter % 2;
        if (i == 1)
          return "site1";
        if (i == 2)
          return DateTime.Now.Date.AddMinutes(counter * 15);
        if (i == 3)
          return precip;

        return -1;
      }
    }

    internal bool Read()
    {
      counter++;
      precip += 0.01f;
      return counter < 100;
    }

    internal void Close()
    {
    }
  }
}
