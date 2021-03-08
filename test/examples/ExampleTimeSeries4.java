import javax.swing.UIManager;

import hec.heclib.dss.*;
import hec.heclib.util.HecTime;
import hec.heclib.util.HecTimeArray;
import hec.io.TimeSeriesContainer;

//  A simple snippit of code to demonstrate storing and retrieving 
//  regular-interval time series data with second granularity 
//  For a complete proper example, see SampleTimeSeriesComplete
//
public class ExampleTimeSeries4 {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}
		
		//  Write a regular interval data set.  Gen up the data
		TimeSeriesContainer tsc = new TimeSeriesContainer();		
		double[] values = new double[200];	
		for (int i=0; i<values.length; i++) {
			values[i] = (double)i;
		}
		tsc.setValues(values);
		HecTime hecTime = new HecTime("20Jan2011", "1200");
		tsc.setStartTime(hecTime);
		//  The E part of "5Seconds" identifies as having second granularity
		tsc.setName("/Basin/Location/Flow//5Seconds/Java Second Granularity/");
		tsc.setUnits("CFS");
		tsc.setType("Inst-Val");
		
		HecTimeSeries dssTimeSeries = new HecTimeSeries();
		dssTimeSeries.setDSSFileName("C:/temp/Example7.dss");
		int status = dssTimeSeries.write(tsc);
		dssTimeSeries.done();		
		if (status != 0) return;
		
		//  Read data
		TimeSeriesContainer tscRead = new TimeSeriesContainer();
		//  Note - implicit time window given using D and E parts.
		//  A time window must be provided and there are several alternatives for doing this
		tscRead.setName("/Basin/Location/Flow/20Jan2011/5Seconds/Java Second Granularity/");
		HecTimeSeries dssTimeSeriesRead = new HecTimeSeries();
		dssTimeSeriesRead.setDSSFileName("C:/temp/Example7.dss");		
		status = dssTimeSeriesRead.read(tscRead, true);
		dssTimeSeriesRead.done();		
		if (status != 0) return;
		//  The times in the time array will automatically use second granularity
		//  Also tsc.timeGranularitySeconds == 1
		HecTimeArray hTimes = tscRead.getTimes();
		double vals[] = tscRead.getValues();
		for (int i=0; i<vals.length; i++) {			
			System.out.println("Ordinate: " + i + ", time: " + hTimes.element(i).dateAndTime() + 
					", value: " + vals[i]);
		}
		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}