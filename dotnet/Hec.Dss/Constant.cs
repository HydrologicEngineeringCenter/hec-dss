namespace Hec.Dss
{
  static class Constant
  {
    public const double UndefinedDouble = -9999.0;

    static public bool IsDefined(double x)
    {
      return x == UndefinedDouble;
    }

  }
}
