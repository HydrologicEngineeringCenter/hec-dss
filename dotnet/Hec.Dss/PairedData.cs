using System;
using System.Collections.Generic;
using System.Data;

namespace Hec.Dss
{
  public class PairedData
  {
    static PairedData() => Assembly.Initialize();

    public DssPath Path { get; set; }

    public double[] Ordinates { get; set; }
    public List<double[]> Values { get; set; }
    public List<string> Labels { get; set; }
    public string TypeIndependent { get; set; }
    public string TypeDependent { get; set; }
    public string UnitsIndependent { get; set; }
    public string UnitsDependent { get; set; }
    public int CurveCount { get { return Values.Count; } }

    public LocationInformation LocationInformation { get; set; }

    public PairedData()
    {
      Labels = new List<string>();
      Values = new List<double[]>();
    }

    public PairedData(double[] xvalues, double[] yvalues, List<string> labels = null, string xunits = "", string xtype = "", string yunits = "", string ytype = "", string path = null)
    {
      Values = new List<double[]>();
      Path = new DssPath(path);
      Ordinates = xvalues;
      Labels = labels;
      Values.Add(yvalues);
      UnitsIndependent = xunits;
      UnitsDependent = yunits;
      TypeIndependent = xtype;
      TypeDependent = ytype;
    }

    public PairedData(double[] xvalues, List<double[]> yvalues, List<string> labels = null, string xunits = "", string xtype = "", string yunits = "", string ytype = "", string path = null)
    {
      Path = new DssPath(path);
      Ordinates = xvalues;
      Labels = labels;
      Values = yvalues;
      UnitsIndependent = xunits;
      UnitsDependent = yunits;
      TypeIndependent = xtype;
      TypeDependent = ytype;
    }

    public DataTable ToDataTable(bool includeIndex = false)
    {
      var dt = new DataTable();
      if (includeIndex)
      {
        dt.Columns.Add("index");
      }
      dt.Columns.Add("stage", typeof(double));

      if (Labels != null && Labels.Count == Values.Count)
      {
        for (int i = 0; i < Labels.Count; i++)
        {
          dt.Columns.Add(Labels[i], typeof(double));
        }
      }
      else
      {
        for (int i = 0; i < Values.Count; i++)
        {
          dt.Columns.Add("value" + (i+1).ToString(), typeof(double));
        }
      }
      

      for (int i = 0; i < CurveCount; i++)
      {
        var row = new object[] { };
        if (includeIndex)
        {
          Array.Resize(ref row, Values.Count + 2);
          row[0] = i + 1;
          row[1] = Ordinates[i];
          for (int j = 0, k = 2; j < Values.Count; j++, k++)
          {
            row[k] = Values[j][i];
          }
          dt.Rows.Add(row);
        }
        else
        {
          Array.Resize(ref row, Values.Count + 1);
          row[0] = Ordinates[i];
          for (int j = 0, k = 1; j < Values.Count; j++, k++)
          {
            row[k] = Values[j][i];
          }
          dt.Rows.Add(row);
        }
      }

      return dt;
    }

    static public PairedData Create(double[] xvalues, double[] yvalues, string xunits = "", string xtype = "", string yunits = "", string ytype = "", string path = null)
    {
      PairedData pd = new PairedData();
      pd.Path = new DssPath(path);
      pd.Ordinates = xvalues;
      pd.Values.Add(yvalues);
      pd.UnitsIndependent = xunits;
      pd.UnitsDependent = yunits;
      pd.TypeIndependent = xtype;
      pd.TypeDependent = ytype;
      return pd;
    }

    static public PairedData Create(double[] xvalues, List<double[]> yvalues, string xunits = "", string xtype = "", string yunits = "", string ytype = "", string path = null)
    {
      PairedData pd = new PairedData();
      pd.Path = new DssPath(path);
      pd.Ordinates = xvalues;
      pd.Values = yvalues;
      pd.UnitsIndependent = xunits;
      pd.UnitsDependent = yunits;
      pd.TypeIndependent = xtype;
      pd.TypeDependent = ytype;
      return pd;
    }

  
  }
}
