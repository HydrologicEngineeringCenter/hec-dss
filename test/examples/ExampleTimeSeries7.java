import javax.swing.UIManager;

import hec.heclib.dss.*;
import hec.heclib.util.HecTime;
import hec.heclib.util.HecTimeArray;
import hec.io.TimeSeriesContainer;

//  Regular interval time series data with single int quality flags 

public class ExampleTimeSeries7 {

	public static void main (String args[])  {		

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}
		
		//  If you have issues, show user debug info
		//  dssTimeSeries.zsetMessageLevel(HecDataManager.MESS_METHOD_GLOBAL, HecDataManager.MESS_LEVEL_USER_DIAG);
		
		//  Write a regular interval data set.  Gen up the data			
		double values[] = new double[200];	
		int quality[] = new int[200];
		for (int i=0; i<values.length; i++) {
			values[i] = (double)i;
			//  set evens to 3, odds to 5
			if ((i & 1) == 0) {
				quality[i] = 3;
			}
			else {
				quality[i] = 5;
			}
		}
		
		TimeSeriesContainer tsc = new TimeSeriesContainer();		
		HecTime hecTime = new HecTime("20Jan2010", "2400");
		tsc.setStartTime(hecTime);
		tsc.setName("/Basin/Location/Flow//1Hour/Quality/");
		tsc.setValues(values);
		tsc.setQuality(quality);
		tsc.units = "CFS";
		tsc.type = "Inst-Val";
		
		HecTimeSeries dssTimeSeries = new HecTimeSeries();	
		dssTimeSeries.setDSSFileName("C:/temp/Example7.dss");		
		int status = dssTimeSeries.write(tsc);
		dssTimeSeries.done();		
		if (status != 0) return;
		
		//  Read data
		TimeSeriesContainer tscRead = new TimeSeriesContainer();
		//  Note - implicit time window given using D and E parts.
		//  A time window must be provided and there are several alternatives for doing this
		tscRead.fullName = "/Basin/Location/Flow/01Jan2010/1Hour/Quality/";
		HecTimeSeries dssTimeSeriesRead = new HecTimeSeries();
		dssTimeSeriesRead.setDSSFileName("C:/temp/Example7.dss");		
		status = dssTimeSeriesRead.read(tscRead, true);
		dssTimeSeriesRead.done();		
		if (status != 0) return;
		HecTimeArray hTimes = tscRead.getTimes();
		double vals[] = tscRead.getValues();
		int qual[] = tscRead.getQuality();
		if ((qual == null) || (vals == null) || (qual.length != vals.length)) {
			System.out.println("No quality or values or length miss match");
			return;
		}
		for (int i=0; i<vals.length; i++) {
			System.out.println("Ordinate: " + i + ", time: " + hTimes.element(i).dateAndTime() + 
					", value: " + vals[i] + ", quality:" + qual[i] );
		}
		HecDataManager.closeAllFiles();
	}
}

