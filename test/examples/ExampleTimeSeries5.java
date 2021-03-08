import javax.swing.UIManager;
import hec.heclib.dss.*;
import hec.heclib.util.HecTime;
import hec.heclib.util.HecTimeArray;
import hec.io.TimeSeriesContainer;

//  A simple snippit of code to demonstrate storing and retrieving 
//  irregular-interval time series data
//
public class ExampleTimeSeries5 {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}
		
		//  Write a irregular interval data set.  Gen up the data and times
		double values[] = new double[200];
		//  Irregular interval data requires the times array
		HecTimeArray times = new HecTimeArray(200);
		HecTime hecTime = new HecTime(HecTime.SECOND_GRANULARITY);
		hecTime.set("20Jan2010", "1200");
		for (int i=0; i<200; i++) {
			values[i] = (double)i;
			//  Make it irregular by adding 10 seconds for even, subtracting for odd
			if (i % 2 == 0) {
				hecTime.addSeconds(20 + 10);
			}
			else {
				hecTime.addSeconds(20 - 10);
			}
			times.setElementAt(new HecTime(hecTime), i);
		}
		TimeSeriesContainer tsc = new TimeSeriesContainer();
		tsc.set(values, times);
		tsc.setName("/Basin/Location/Flow//Ir-Month/Second Granularity/");		
		tsc.setUnits("CFS");
		tsc.setType("Inst-Val");
		
		HecTimeSeries dssTimeSeries = new HecTimeSeries();
		dssTimeSeries.setDSSFileName("C:/temp/Example7.dss");
		dssTimeSeries.zsetMessageLevel(HecDataManager.MESS_METHOD_GLOBAL, HecDataManager.MESS_LEVEL_USER_DIAG);
		int status = dssTimeSeries.write(tsc);
		dssTimeSeries.done();		
		if (status != 0) return;
		
		//  Read data
		TimeSeriesContainer tscRead = new TimeSeriesContainer();		
		tscRead.setName("/Basin/Location/Flow//Ir-Month/Second Granularity/");
		tscRead.setStartTime(new HecTime("01Jan2010", "0001"));
		tscRead.setEndTime(new HecTime("31Jan2010", "2400"));
		HecTimeSeries dssTimeSeriesRead = new HecTimeSeries();
		dssTimeSeriesRead.setDSSFileName("C:/temp/Example7.dss");		
		status = dssTimeSeriesRead.read(tscRead, false);
		dssTimeSeriesRead.done();		
		if (status != 0) return;
		HecTimeArray hTimes = tscRead.getTimes();
		double vals[] = tscRead.getValues();
		for (int i=0; i<vals.length; i++) {			
			System.out.println("Ordinate: " + i + ", time: " + hTimes.element(i).dateAndTime() + 
					", value: " + vals[i]);
		}
		HecDataManager.closeAllFiles();
	}
}
