import javax.swing.UIManager;

import hec.heclib.dss.*;
import hec.heclib.util.HecTime;
import hec.heclib.util.HecTimeArray;
import hec.io.TimeSeriesContainer;

//  A simple snipit of code to demonstrate storing and retrieving 
//  regular-interval time series data
//  For a complete proper example, see SampleTimeSeriesComplete
//
public class ExampleTimeSeries1 {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}
		
		//  Write a regular interval data set.  Gen up the data
		TimeSeriesContainer tsc = new TimeSeriesContainer();		
		double[] values = new double[200];
		for (int i=0; i<200; i++) {
			values[i] = (double)i / 3.0;
		}
		tsc.setValues(values);
		tsc.setName("/Basin/Location/Flow//1Hour/Java Sample/");
		HecTime hecTime = new HecTime("20Jan2010", "1230");
		tsc.setStartTime(hecTime);		
		tsc.setUnits("CFS");
		tsc.setType("Inst-Val");
		
		HecTimeSeries dssTimeSeries = new HecTimeSeries();
		dssTimeSeries.setDSSFileName("C:/temp/Example7.dss");
		//  If you have issues, show user debug info
		//  dssTimeSeries.zsetMessageLevel(HecDataManager.MESS_METHOD_GLOBAL, HecDataManager.MESS_LEVEL_USER_DIAG);   
		int status = dssTimeSeries.write(tsc);
		dssTimeSeries.done();		
		if (status != 0) return;
		
		//  Read data
		TimeSeriesContainer tscRead = new TimeSeriesContainer();
		//  Note - implicit time window given using D and E parts.
		//  A time window must be provided and there are several alternatives for doing this
		tscRead.setName("/Basin/Location/Flow/01Jan2010/1Hour/Java Sample/");
		HecTimeSeries dssTimeSeriesRead = new HecTimeSeries();
		dssTimeSeriesRead.setDSSFileName("C:/temp/Example7.dss");
		status = dssTimeSeriesRead.read(tscRead, true);
		dssTimeSeriesRead.done();		
		if (status != 0) return;
		HecTimeArray hTimes = tscRead.getTimes();
		double vals[] = tscRead.getValues();
		for (int i=0; i<vals.length; i++) {			
			System.out.println("Ordinate: " + i + ", time: " + hTimes.element(i).dateAndTime() + 
					", value: " + vals[i]);
		}
		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
