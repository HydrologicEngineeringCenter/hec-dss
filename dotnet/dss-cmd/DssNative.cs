﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

public class DssNative
{
  [DllImport("hecdss")]
  public static extern int hec_dss_CONSTANT_MAX_PATH_SIZE();

  [DllImport("hecdss")]
   public static extern int hec_dss_open(string fileName, out IntPtr dss);

   [DllImport("hecdss")]
   public static extern int hec_dss_open(string fileName, out IntPtr dss);

   [DllImport("hecdss")]
   public static extern int hec_dss_close(IntPtr dss);

   [DllImport("hecdss")]
   public static extern int hec_dss_version(IntPtr dss);

   [DllImport("hecdss")]
   public static extern int hec_dss_record_count(IntPtr dss);

   [DllImport("hecdss")]
   public static extern IntPtr hec_dss_deprecated_ifltab(IntPtr dss);

   [DllImport("hecdss")]
   public static extern void hec_dss_deprecated_ifltab_print(IntPtr ifltab);


   [DllImport("hecdss", CharSet = CharSet.Ansi, ExactSpelling = true)]
   public static extern int hec_dss_tsRetrieve(IntPtr dss, string pathname,
                                     string startDate, string startTime,
                                     string endDate, string endTime,
                                     int[] timeArray, double[] valueArray, int arraySize,
                                     ref int numberValuesRead, ref int julianBaseDate,
                                     byte[] units, int unitsLength, byte[] type, int typeLength);


  [DllImport("hecdss")]
  internal static extern int hec_dss_tsGetDateTimeRange(IntPtr dss, string pathname, int boolFullSet,
                                            ref int firstValidJulian, ref int firstSeconds,
                                            ref int lastValidJulian, ref int lastSeconds);

   [DllImport("hecdss", CharSet = CharSet.Ansi, ExactSpelling = true)] 
   public static extern int hec_dss_tsGetSizes(IntPtr dss, string pathname,
                                    string startDate, string startTime, 
                                    string endDate, string endTime,
                                    ref int numberValues);

  [DllImport("hecdss", CharSet = CharSet.Ansi, ExactSpelling = true)]
  public static extern int hec_dss_catalog(IntPtr dss, byte[] pathBuffer,int[] recordTypes,int count, int pathBufferItemSize);
}

