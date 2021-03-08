import javax.swing.UIManager;

import hec.heclib.dss.*;
import hec.heclib.util.HecTime;
import hec.heclib.util.HecTimeArray;
import hec.io.TimeSeriesContainer;

//  Regular interval time series data with multiple int quality flags and character notes 

public class ExampleTimeSeries8 {

	public static void main (String args[])  {
		String alpha = "abcdefghijklmnopqrstuvwxyz";

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}
		
		//  Write a regular interval data set.  Gen up the data
		TimeSeriesContainer tsc = new TimeSeriesContainer();	
		tsc.setName("/Basin/Location/Flow//1Hour/Quality and Notes/");
		double values[] = new double[200];
		//  Use "quality7[][]" for multiple int quality flags.
		//  Cannot use both quality and quality7, as they both occupy the same space on disk. 
		int quality7[][] = new int[200][4];
		String cnotes[] = new String[200];
		for (int i=0; i<200; i++) {
			values[i] = (double)i;			
			int n = i/25;
			n = i - (n*25) + 1;
			cnotes[i] = alpha.substring(0, n);
			for (int j=0; j<4; j++) {
				quality7[i][j] = (i * 10) + j;
			}
		}
		tsc.setStartTime(new HecTime("20Jan2010", "2400"));	
		tsc.setValues(values);
		tsc.setQuality7(quality7);
		tsc.setCharacterNotes(cnotes);
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
		tscRead.setName("/Basin/Location/Flow/01Jan2010/1Hour/Quality and Notes/");
		HecTimeSeries dssTimeSeriesRead = new HecTimeSeries();
		dssTimeSeriesRead.setDSSFileName("C:/temp/Example7.dss");		
		status = dssTimeSeriesRead.read(tscRead, true);
		dssTimeSeriesRead.done();		
		if (status != 0) return;
		double vals[] = tsc.getValues();
		HecTimeArray hTimes = tscRead.getTimes();
		String notes[] = tsc.getCharacterNotes();
		int qual[][] = tsc.getQuality7();
		//  Note - checking of items and lengths would be done here
		for (int i=0; i<vals.length; i++) {
			System.out.println("Ordinate: " + i + ", time: " + hTimes.element(i).dateAndTime() + 
					", value: " + vals[i] +  ", note: " + notes[i]);
			System.out.println("     Quality = " + qual[i][0] + ",  " + 
					qual[i][1] + ",  " + qual[i][2] + ",  " + qual[i][3]);
		}
		HecDataManager.closeAllFiles();
	}
}

