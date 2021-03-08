import javax.swing.UIManager;

import hec.heclib.dss.*;
import hec.heclib.util.HecTime;
import hec.heclib.util.HecTimeArray;
import hec.io.TimeSeriesContainer;

//  Store time series profile data
//  In this example, we'll use regular-interval without quality or notes
//We'll store 20 depths for 1000 days
//
public class ExampleTimeSeries9 {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}
					
		double profileDepths[] = new double[20];
		double profileValues[][] = new double[1000][20];
		int numberDepths = 20;
		int numberValues = 1000;
		//  measurements every 3 meters
		for (int i=0; i<numberDepths; i++) {
			profileDepths[i] = (double)(i * 3);
		}
		for (int i=0; i<numberValues; i++) {
			for (int j=0; j<numberDepths; j++) {
				profileValues[i][j] = 10 * (Math.sin(i) + Math.sin(j) + 1);
			}
		}
		TimeSeriesContainer tsc = new TimeSeriesContainer();
		tsc.setName("/Basin/Location/Depth-Temperature//1Day/Java Profile Sample/");
		tsc.setStartTime(new HecTime("20Jan2010", "1200"));	
		tsc.setProfile(profileDepths, profileValues);
		tsc.setProfileDepthsUnits("Meters");
		tsc.setProfileValuesUnits("deg C");
		tsc.setType("Inst-Val");
	
		HecTimeSeries dssTimeSeries = new HecTimeSeries();
		dssTimeSeries.setDSSFileName("C:/temp/Example7.dss");
		int status = dssTimeSeries.write(tsc);
		dssTimeSeries.done();		
		if (status != 0) return;
		
		//  Read data
		TimeSeriesContainer tscRead = new TimeSeriesContainer();
		tscRead.setName("/Basin/Location/Depth-Temperature//1Day/Java Profile Sample/");
		HecTime hecTime = new HecTime("20Jan2010", "1200");
		tscRead.setStartTime(hecTime);
		hecTime.addDays(999);   //  999 days is 1000 values
		tscRead.setEndTime(hecTime);
		HecTimeSeries dssTimeSeriesRead = new HecTimeSeries();
		dssTimeSeriesRead.setDSSFileName("C:/temp/Example7.dss");
		
		status = dssTimeSeriesRead.read(tscRead, true);
		dssTimeSeriesRead.done();		
		if (status != 0) return;
		numberValues = tscRead.getNumberValues();
		HecTimeArray hecTimeArray = tscRead.getTimes();
		double profiles[][] = tscRead.getProfileValues();
		double depths[] = tscRead.getProfileDepths();
		for (int i=0; i<depths.length; i++) {
			System.out.println("Ordinate: " + i + ", Depth: " + depths[i]);
		}
		for (int i=0; i<numberValues; i++) {
			System.out.println("Ordinate: " + i + ", time: " + hecTimeArray.elementAt(i).dateAndTime() + 
					", First temp: " + profiles[i][0] + ", second: " + 
					profiles[i][1]	+ ", third: " + profiles[i][2]);
		}
		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
