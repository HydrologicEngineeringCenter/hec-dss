using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Text.RegularExpressions;
using Hec.Dss.Native;

namespace Hec.Dss
{
  public static class PathAssist
  {

    public static List<string> FilterByPart(IList<string> pathList, DssPath pathFilter)
    {
      return FilterByPart(pathList, pathFilter.Apart, pathFilter.Bpart, pathFilter.Cpart, pathFilter.Dpart, pathFilter.Epart, pathFilter.Fpart);
    }

    /// <summary>
    /// Fill in each part you want to search for. To search for any word in a parameter, leave it as "".  You can give any part as a regular expression.  For example to partially match a part, type it in partially and append .* to the end of it.
    /// For help: <a href="https://regex101.com/r/NrjUWv/1">https://regex101.com/r/NrjUWv/1</a>
    /// </summary>
    /// <param name="ListOfPaths"></param>
    /// <param name="aPart"></param>
    /// <param name="bPart"></param>
    /// <param name="cPart"></param>
    /// <param name="dPart"></param>
    /// <param name="ePart"></param>
    /// <param name="fPart"></param>
    /// <returns></returns>
    public static List<string> FilterByPart(IList<string> ListOfPaths, string aPart = "", string bPart = "", string cPart = "", string dPart = "", string ePart = "", string fPart = "")
    {
      List<string> results = new List<string>();
      Match match;
      aPart = aPart == "" ? "(.*?)" : Regex.Escape(aPart);
      bPart = bPart == "" ? "(.*?)" : Regex.Escape(bPart);
      cPart = cPart == "" ? "(.*?)" : Regex.Escape(cPart);
      dPart = dPart == "" ? "(.*?)" : Regex.Escape(dPart);
      ePart = ePart == "" ? "(.*?)" : Regex.Escape(ePart);
      fPart = fPart == "" ? "(.*?)" : Regex.Escape(fPart);
      Regex regex = new Regex("/" + aPart + "/" + bPart + "/" + cPart  + "/" + dPart +  "/" + ePart +  "/" + fPart +  "/");
      for (int i = 0; i < ListOfPaths.Count;i++)
      {
        match = regex.Match(ListOfPaths[i]);
        if (match.Success)
          results.Add(match.Value);
      }
      return results;
    }

    /// <summary>
    /// Fill in each part you want to search for. To search for any word in a parameter, leave it as "".  You can give any part as a regular expression.  For example to partially match a part, type it in partially and append .* to the end of it.
    /// For help: <a href="https://regex101.com/r/NrjUWv/1">https://regex101.com/r/NrjUWv/1</a>
    /// </summary>
    /// <param name="ListOfPaths"></param>
    /// <param name="aPart"></param>
    /// <param name="bPart"></param>
    /// <param name="cPart"></param>
    /// <param name="dPart"></param>
    /// <param name="ePart"></param>
    /// <param name="fPart"></param>
    /// <returns></returns>
    public static List<DssPath> FilterByPart(IList<DssPath> ListOfPaths, string aPart = "", string bPart = "", string cPart = "", string dPart = "", string ePart = "", string fPart = "")
    {
      List<DssPath> results = new List<DssPath>();
      Match match;
      aPart = aPart == "" ? "(.*?)" : Regex.Escape(aPart);
      bPart = bPart == "" ? "(.*?)" : Regex.Escape(bPart);
      cPart = cPart == "" ? "(.*?)" : Regex.Escape(cPart);
      dPart = dPart == "" ? "(.*?)" : Regex.Escape(dPart);
      ePart = ePart == "" ? "(.*?)" : Regex.Escape(ePart);
      fPart = fPart == "" ? "(.*?)" : Regex.Escape(fPart);
      Regex regex = new Regex("/" + aPart + "/" + bPart + "/" + cPart + "/" + dPart + "/" + ePart + "/" + fPart + "/");
      for (int i = 0; i < ListOfPaths.Count; i++)
      {
        match = regex.Match(ListOfPaths[i].FullPath);
        if (match.Success)
          results.Add(ListOfPaths[i]);
      }
      return results;
    }

    /// <summary>
    /// Gets all the unique A Parts in a list of paths
    /// </summary>
    /// <param name="ListOfPaths"></param>
    /// <returns>A list of all the unique A Parts in the list of paths</returns>
    public static List<string> GetUniqueAParts(IList<DssPath> ListOfPaths)
    {
      var hs = new HashSet<string>();
      foreach (DssPath path in ListOfPaths)
      {
        hs.Add(path.Apart);
      }
      return hs.ToList();
    }


    /// <summary>
    /// Gets all the unique B Parts in a list of paths
    /// </summary>
    /// <param name="ListOfPaths"></param>
    /// <returns>A list of all the unique B Parts in the list of paths</returns>
    public static List<string> GetUniqueBParts(IList<DssPath> ListOfPaths)
    {
      var hs = new HashSet<string>();
      foreach (DssPath path in ListOfPaths)
      {
        hs.Add(path.Bpart);
      }
      return hs.ToList();
    }

    /// <summary>
    /// Gets all the unique C Parts in a list of paths
    /// </summary>
    /// <param name="ListOfPaths"></param>
    /// <returns>A list of all the unique C Parts in the list of paths</returns>
    public static List<string> GetUniqueCParts(IList<DssPath> ListOfPaths)
    {
      var hs = new HashSet<string>();
      foreach (DssPath path in ListOfPaths)
      {
        hs.Add(path.Cpart);
      }
      return hs.ToList();
    }

    /// <summary>
    /// Gets all the unique D Parts in a list of paths
    /// </summary>
    /// <param name="ListOfPaths"></param>
    /// <returns>A list of all the unique D Parts in the list of paths</returns>
    public static List<string> GetUniqueDParts(IList<DssPath> ListOfPaths)
    {
      var hs = new HashSet<string>();
      foreach (DssPath path in ListOfPaths)
      {
        hs.Add(path.Dpart);
      }
      return hs.ToList();
    }

    /// <summary>
    /// Gets all the unique E Parts in a list of paths
    /// </summary>
    /// <param name="ListOfPaths"></param>
    /// <returns>A list of all the unique E Parts in the list of paths</returns>
    public static List<string> GetUniqueEParts(IList<DssPath> ListOfPaths)
    {
      var hs = new HashSet<string>();
      foreach (DssPath path in ListOfPaths)
      {
        hs.Add(path.Epart);
      }
      return hs.ToList();
    }

    /// <summary>
    /// Gets all the unique F Parts in a list of paths
    /// </summary>
    /// <param name="ListOfPaths"></param>
    /// <returns>A list of all the unique F Parts in the list of paths</returns>
    public static List<string> GetUniqueFParts(IList<DssPath> ListOfPaths)
    {
      var hs = new HashSet<string>();
      foreach (DssPath path in ListOfPaths)
      {
        hs.Add(path.Fpart);
      }
      return hs.ToList();
    }

    /// <summary>
    /// Gets all unique record types in a list of paths
    /// </summary>
    /// <param name="ListOfPaths"></param>
    /// <returns>A list of all unique record types in the list of paths</returns>
    public static List<RecordType> GetUniqueRecordTypeNames(IList<DssPath> ListOfPaths)
    {
      var hs = new HashSet<RecordType>();
      foreach (DssPath path in ListOfPaths)
      {
        hs.Add(path.RecordType);
      }
      return hs.ToList();
    }

    /// <summary>
    /// Gets all unique data types in a list of paths
    /// </summary>
    /// <param name="ListOfPaths"></param>
    /// <returns>A list of all unique data types in the list of paths</returns>
    public static List<string> GetUniqueDataTypes(IList<DssPath> ListOfPaths)
    {
      var hs = new HashSet<string>();
      foreach (DssPath path in ListOfPaths)
      {
        hs.Add(path.DataType);
      }
      return hs.ToList();
    }

    /// <summary>
    /// Gets all unique units in a list of paths
    /// </summary>
    /// <param name="ListOfPaths"></param>
    /// <returns>A list of all unique units in the list of paths</returns>
    public static List<string> GetUniqueUnits(IList<DssPath> ListOfPaths)
    {
      var hs = new HashSet<string>();
      foreach (DssPath path in ListOfPaths)
      {
        hs.Add(path.Units);
      }
      return hs.ToList();
    }

    /// <summary>
    /// Gets all unique X vals in a list of paths
    /// </summary>
    /// <param name="ListOfPaths"></param>
    /// <returns>A list of all unique Xs in the list of paths</returns>
    public static List<double> GetUniqueXs(IList<DssPath> ListOfPaths)
    {
      var hs = new HashSet<double>();
      foreach (DssPath path in ListOfPaths)
      {
        hs.Add(path.XOrdinate);
      }
      return hs.ToList();
    }

    /// <summary>
    /// Gets all unique Y vals in a list of paths
    /// </summary>
    /// <param name="ListOfPaths"></param>
    /// <returns>A list of all unique Ys in the list of paths</returns>
    public static List<double> GetUniqueYs(IList<DssPath> ListOfPaths)
    {
      var hs = new HashSet<double>();
      foreach (DssPath path in ListOfPaths)
      {
        hs.Add(path.YOrdinate);
      }
      return hs.ToList();
    }

    /// <summary>
    /// Gets all unique Z vals in a list of paths
    /// </summary>
    /// <param name="ListOfPaths"></param>
    /// <returns>A list of all unique Zs in the list of paths</returns>
    public static List<double> GetUniqueZs(IList<DssPath> ListOfPaths)
    {
      var hs = new HashSet<double>();
      foreach (DssPath path in ListOfPaths)
      {
        hs.Add(path.ZOrdinate);
      }
      return hs.ToList();
    }
  }
}
