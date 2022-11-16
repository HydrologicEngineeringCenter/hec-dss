using System;
using System.Text.RegularExpressions;
using System.Xml;
using System.Xml.Linq;

namespace Hec.Dss
{
  /// <summary>
  ///VerticalDatumInfo holds datum offset, and related information.
  ///offsets are added to an elevation in the native datum
  ///units are specifed in the vertical-datum-info attribute 'units'
  ///
  /// based on VerticalDatumContainer.java by Mike Perryman
  ///
  /// Karl Tarbet, September 2019 
  /// </summary>
  public class VerticalDatumInfo
  {

    public VerticalDatumInfo()
    {

    }
    /*
     <vertical-datum-info unit="ft">
<native-datum>NGVD-29</native-datum>
<elevation>615.2</elevation>
<offset estimate="false">
<to-datum>NGVD-29</to-datum>

<value>0.0</value></offset>
<offset estimate="true">
<to-datum>NAVD-88</to-datum><value>0.5</value>
</offset></vertical-datum-info>
     */

    public string ToXml(bool humanFriendly = false)
    {
      XmlDocument doc = new XmlDocument();
      XmlElement vertical_datum = doc.CreateElement("vertical-datum-info");

      var node = doc.AppendChild(vertical_datum);
      var a = doc.CreateAttribute("unit");
      a.Value = Units;
      vertical_datum.Attributes.Append(a);


      var elev = doc.CreateElement("elevation");
      elev.InnerText = Elevation.ToString();
      vertical_datum.AppendChild(elev);

      var nativeDatum = doc.CreateElement("native-datum");
      nativeDatum.InnerText = NativeDatum;
      vertical_datum.AppendChild(nativeDatum);

      AppendOffset(doc, "NGVD-29", NGVD29OffsetIsEstimate, NGVD29Offset);
      AppendOffset(doc, "NAVD-88", NAVD88OffsetIsEstimate, NAVD88Offset);

      if (humanFriendly)
      {
        XDocument xd = XDocument.Parse(ToXml());
        return xd.ToString();
      }

      return doc.OuterXml;
    }

    private void AppendOffset(XmlDocument doc, string offsetName, bool isEstimate, double offsetValue)
    {
      var offset = doc.CreateElement("offset");
      var a = doc.CreateAttribute("estimate");
      a.Value = isEstimate.ToString().ToLower();
      offset.Attributes.Append(a);

      var to_datum = doc.CreateElement("to-datum");
      to_datum.InnerText = offsetName.ToString();
      offset.AppendChild(to_datum);


      var val = doc.CreateElement("value");
      val.InnerText = offsetValue.ToString();
      offset.AppendChild(val);

      doc.FirstChild.AppendChild(offset);
    }

    public String NativeDatum = "";
    //public String CurrentDatum = "";
    public String LocalDatumName = "";
    public String Units = "";
    public double Elevation = Constant.UNDEFINED_VERTICAL_DATUM_VALUE;
    public double NGVD29Offset = Constant.UNDEFINED_VERTICAL_DATUM_VALUE;
    public double NAVD88Offset = Constant.UNDEFINED_VERTICAL_DATUM_VALUE;
    public bool NGVD29OffsetIsEstimate = true;
    public bool NAVD88OffsetIsEstimate = true;

    public VerticalDatumInfo(string xmlFragement)
    {
      try
      {
        XmlDocument doc = new XmlDocument();
        doc.LoadXml(xmlFragement);
        var root = doc.DocumentElement;

        var s = GetInnerText(root, "/vertical-datum-info/elevation");
        if (s.Trim().Length > 0 && double.TryParse(s, out double elev))
        {
          Elevation = elev;
        }

        var n = root.SelectSingleNode("/vertical-datum-info");
        Units = GetAttribute(n, "unit", "");

        NativeDatum = GetInnerText(root, "/vertical-datum-info/native-datum");
        ReadOffsets(root);

      }
      catch (Exception eek)
      {
        Console.WriteLine("Error: parsing xml vertical-datum-info " + eek.Message);
      }

    }

    private void ReadOffsets(XmlElement root)
    {
      var nodes = root.SelectNodes("/vertical-datum-info/offset");

      for (int i = 0; i < nodes.Count; i++)
      {
        var n = nodes[i];

        if (n["to-datum"] != null && n["value"] != null)
        {
          var to_datum = n["to-datum"].InnerText;
          var val = n["value"].InnerText;
          var isEstimate = GetAttribute(n, "estimate", "true");

          if (bool.TryParse(isEstimate, out bool estimate)
              && double.TryParse(val, out double offsetValue))
          {

            if (Regex.IsMatch(to_datum, "NGVD\\s*(-?)\\s*29"))
            {
              NGVD29OffsetIsEstimate = estimate;
              NGVD29Offset = offsetValue;
            }
            else if (Regex.IsMatch(to_datum, "NAVD\\s*(-?)\\s*88"))
            {
              NAVD88OffsetIsEstimate = estimate;
              NAVD88Offset = offsetValue;
            }
          }
        }
      }
    }

    private static string GetAttribute(XmlNode n, string name, string defaultValue)
    {
      if (n.Attributes.Count == 0)
        return defaultValue;

      var x = n.Attributes[name];
      if (x != null)
        return x.Value;

      return defaultValue;
    }

    private string GetInnerText(XmlElement root, string path)
    {
      var node = root.SelectSingleNode(path);
      return GetInnerText(node);
    }

    private static string GetInnerText(XmlNode node)
    {
      if (node == null)
        return "";
      if (node.InnerText != "")
        return node.InnerText;
      return "";
    }


  }
}
