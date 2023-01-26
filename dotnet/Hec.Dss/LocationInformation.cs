using System.Collections.Generic;
using System.Text;
using Hec.Dss.Native;

namespace Hec.Dss
{
  public enum CoordinateSystem { None = 0, LatLong = 1, StatePlaneFIPS = 2, StatePlanADS = 3, UTM = 4, Local = 5 }
  public class LocationInformation
  {
    static LocationInformation() => Assembly.Initialize();

    public const string VERTICAL_DATUM_INFO_KEY = "verticalDatumInfo";

    public LocationInformation()
    {
      CoordinateSystem = CoordinateSystem.None;
      TimeZoneName = "";
      Supplemental = "";
    }

    /// <summary>
    /// Longitude, Easting or decimal degrees (negative for Western Hemisphere) 
    /// </summary>
    public double XOrdinate;
    /// <summary>
    /// Latitude, Northing or decimal degrees
    /// </summary>
    public double YOrdinate;
    /// <summary>
    /// Elevation
    /// </summary>
    public double ZOrdiante;
    /// <summary>
    /// 0 - no coordinates set
    /// 1 - Lat/long
    /// 2 - State Plane, FIPS
    /// 3 - State Plane, ADS
    /// 4 - UTM
    /// 5 - Local (other)
    /// </summary>
    public CoordinateSystem CoordinateSystem = CoordinateSystem.None;
    /// <summary>
    /// UTM zone #, or FIPS SPCS # ADS SPCS #
    /// </summary>
    public int CoordinateID;
    /// <summary>
    /// 0 - unspecified
    /// 1 - feet
    /// 2 - meters
    /// 3 - Decimal Degrees
    /// 4 - Degrees Minutes Seconds
    /// </summary>
    public int HorizontalUnits;
    /// <summary>
    /// 0 - unset
    /// 1 - NAD83
    /// 2 - NAD27
    /// 3 - WGS84
    /// 4 - WGS72
    /// 5 - Local (other)
    /// </summary>
    public int HorizontalDatum;
    /// <summary>
    /// 0 - unspecified
    /// 1 - feet
    /// 2 - meters
    /// </summary>
    public int VerticalUnits;
    /// <summary>
    /// 0 - unset
    /// 1 - NAVD88
    /// 2 - NGVD29
    /// 3 - Local (other)
    /// </summary>
    public int VerticalDatum;
    /// <summary>
    /// Location time zone, not necessarily time zone associated with data (maybe GMT?)
    /// e.g., "PST" or "America-Los Angeles".  Never "PDT" (daylight)
    /// </summary>
    public string TimeZoneName;
    /// <summary>
    /// Additional information about the location
    /// data is stored in key:value;key1:value2;
    /// This is a null-terminated string
    /// Separate "pieces" of information should be separated by
    /// a new-line character '\n'
    /// </summary>
    public string Supplemental;


    private Dictionary<string, string> SupplementalAsDictionary()
    {
      Dictionary<string, string> d = new Dictionary<string, string>();

      if (Supplemental == null || Supplemental == "")
        return d;

      var tokens = Supplemental.Split(';');
      for (int i = 0; i < tokens.Length; i++)
      {
        var parts = tokens[i].Split(':');
        if (parts.Length == 2)
        {
          d.Add(parts[0], parts[1]);
        }
      }
      return d;
    }

    private void DictionaryToSupplemental(Dictionary<string, string> dictionary)
    {
      StringBuilder sb = new StringBuilder();
      foreach (var item in dictionary)
      {
        sb.Append(item.Key + ":" + item.Value + ";");
      }
      Supplemental = sb.ToString();
    }


    /// <summary>
    /// data is stored in key:value;key1:value2;....
    /// verticalDatumInfo:H4sIAAAAAAAAAHXQwQrCMAwG4PueouzedQ4mE2phIHjz6D1oCoWuhTXt89taUNR5C/8fvkBkwpXMDSy/A8WFG6c9i87QsdXUqoYx6YBMwtqry/l64sNBio+0rKHFlDPv1H43doMU76DUXuuAxDCQWYAw82ADPi/klvy3/0rqQgIbUfVdL0Udiykquu3TGjf4OfPT9JcfN/kc/n5JNQ84o4x9PAEAAA==;
    /// </summary>
    /// <param name="name"></param>
    /// <returns></returns>
    private string GetCustomValue(string name)
    {
      var d = SupplementalAsDictionary();

      if (!d.ContainsKey(name))
        return "";

      if (name == VERTICAL_DATUM_INFO_KEY)
      {// special case,  need to decode base 64 and unzip
        var rval = Compression.UnCompress(d[name]);
        return rval;
      }
      else
      {
        return d[name];
      }
    }

    private void SetCustomValue(string name, string value)
    {
      if (name.Trim() == "")
        return;
      var d = SupplementalAsDictionary();

      if (value == "")
      {
        d.Remove(name);
      }
      else
      {
        if (name == VERTICAL_DATUM_INFO_KEY)
        {
          d[name] = Compression.Compress(value);
        }
        else
        {
          d[name] = value; //  adds or overwrites
        }
      }
      DictionaryToSupplemental(d);
    }

    /// <summary>
    /// example:
    ///  vertical-datum-info office="SWT" unit="ft">
    /*
       vertical-datum-info office="SWT" unit="ft">
<location>PENS</location>
<native-datum>local</native-datum>
<elevation>757</elevation>
<offset estimate="false">
<to-datum>ngvd29</to-datum>
<value>1.07</value>
</offset>
<offset estimate="true">
<to-datum>NAVD-88</to-datum>
<value>0.3869</value>
</offset>
</vertical-datum-info>*/
    /// </summary>
    public VerticalDatumInfo VerticalDatumInfo
    {
      get
      {

        var xml = GetCustomValue("verticalDatumInfo");
        if (xml != "")
        {
          var rval = new VerticalDatumInfo(xml);
          return rval;
        }

        return new VerticalDatumInfo("");
      }
      set
      {
        // update supplemential infomation string with new 
        SetCustomValue("verticalDatumInfo", value.ToXml());
      }
    }


  }
}
