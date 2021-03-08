import javax.swing.UIManager;
import hec.dataTable.HecDataTableFrame;
import hec.heclib.dss.*;
import hec.heclib.util.HecTime;
import hec.heclib.util.HecTimeArray;
import hec.io.TimeSeriesContainer;

//  A simple snippit of code to demonstrate storing and retrieving 
//  dataset with very large time spans
//  Be carefull not to exceed memory space!
public class ExampleTimeSeries6 {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}

		
		//  Write a irregular interval data set.  Gen up the data and times
		//  We are going to be storing data from the year 1,000 to 55,758
		//  and store one value every 10 days for 2,000,000 values
		int numberValues = 2000000;
		TimeSeriesContainer tsc = new TimeSeriesContainer();
		HecTime hecTime = new HecTime("19Jan0201", "2400");
		HecTimeArray timeArray = new HecTimeArray(numberValues);
		double values[] = new double[numberValues];
		//  Irregular interval data requires the times array
		for (int i=0; i<numberValues; i++) {
			values[i] = (double)i;
			timeArray.setElementAt(new HecTime(hecTime), i);
			hecTime.addDays(10);  //  one value every 10 days	
		}		
		HecDataManager.setDefaultDSSFileName("C:/temp/Example7.dss");
		String pathname = "/Basin/Location/Flow//Ir-Century/Extended dates example 1/";
		tsc.setName(pathname);
		tsc.setUnits("CFS");
		tsc.setType("Inst-Val");
		tsc.set(values, timeArray);

		HecTimeSeries dssTimeSeries = new HecTimeSeries();
		int status = dssTimeSeries.write(tsc);
		dssTimeSeries.done();		
		if (status != 0) return;

		//  Read data
		TimeSeriesContainer tscRead = new TimeSeriesContainer();		
		tscRead.setName(pathname);		
		tscRead.retrieveAllTimes = true;
		HecTimeSeries dssTimeSeriesRead = new HecTimeSeries();	
		status = dssTimeSeriesRead.read(tscRead, true);
		dssTimeSeriesRead.done();		
		if (status != 0) return;

		//  Show in a table
		HecDataTableFrame tableFrame = new HecDataTableFrame(null);
		tableFrame.setData(tscRead);
		tableFrame.setVisible(true);
		
		HecDataManager.closeAllFiles();
	}
}
