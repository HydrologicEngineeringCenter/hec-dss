import javax.swing.JOptionPane;
import javax.swing.UIManager;

import hec.dataTable.HecDataTableFrame;
import hec.heclib.dss.*;
import hec.heclib.util.HecTime;
import hec.heclib.util.HecTimeArray;
import hec.heclib.util.intContainer;
import hec.io.TimeSeriesContainer;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.Vector;
import java.io.File;

//  A simple snipit of code to demonstrate storing and retrieving 
//  dataset with very large time spans
//  Be carefull not to exceed memory space!
public class ExampleTimeSeries11a {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}
		

		
		System.out.println("\nBegin Example Time Series.\n");

		String dssFile="C:/temp/Bulletin_17C_Examples.dss";
		String dssPath="/Santa Cruz River/Lochiel/FLOW-ANNUAL PEAK/01JAN1900/IR-CENTURY/USGS/";
		TimeSeriesContainer eventContainer = new TimeSeriesContainer();
		HecTimeSeries ts = new HecTimeSeries();
		ts.setDSSFileName(dssFile);
		ts.setPathname(dssPath);
		ts.setRetrieveAllTimes(true);
		HecTime undefinedTime = new HecTime();
		ts.setTimeWindow(undefinedTime, undefinedTime);
		ts.read(eventContainer, false);
		ts.closeDSSFile();
		System.out.println( eventContainer.getStartTime().toString());
		System.out.println( eventContainer.getEndTime().toString());

		System.out.println("\nEnd Example Time Series.\n");
		if (ts != null) return ;
		
/*		String fileIn = "C:/temp/MikeB/HECDSS.log";
		String fileOut = "C:/temp/MikeB/HECDSS.txt";
		BufferedReader reader = null;
		BufferedWriter writer = null;
		try{
			File in = new File(fileIn);
			File out = new File(fileOut);
			reader = new BufferedReader(new FileReader(in));
			writer = new BufferedWriter(new FileWriter(out));
			String line;
			while ((line = reader.readLine()) != null){
				if (line.indexOf("zcatalog") == -1) {
					writer.write(line);
					writer.newLine();
				}
			}
			reader.close();
			writer.flush();
			writer.close();
		}
		catch(Exception e) {
			
		}
		
		*/

		//  Write a irregular interval data set.  Gen up the data and times
		//  and store one value every 10 days for 2,000,000 values

		int status;
		HecTime hecTime = new HecTime();
		String DSSFileName = "C:/temp/Test7.dss";
		//HecDataManager.setMessageFile("C:/temp/test7.txt");
		HecDataManager.setDefaultDSSFileName(DSSFileName);
		//HecDataManager.zsetMessageLevel(HecTimeSeries.MESS_METHOD_TS_WRITE, HecTimeSeries.MESS_LEVEL_INTERNAL_DIAG_2);
		
		HecTimeSeries dssTimeSeries = new HecTimeSeries(); 
		
		Vector<String> pathnames = new Vector<String>();
		Vector<Long> updateTimes = new Vector<Long>();
		Vector<Integer> recordTypes = new Vector<Integer>();
		
		long startTime = 1538580911518L;
		
		int st =  dssTimeSeries.recordsUpdated(startTime,  pathnames,
				updateTimes, recordTypes) ;

		HecDataManager.setMessageLevel(50);   
        TimeSeriesContainer tsc = new TimeSeriesContainer();    

        tsc.values = new double[1];
        tsc.quality = new int[1];               
        tsc.numberValues = 1;   
        tsc.values[0] = 2.98;
        tsc.quality[0] = 5;

        hecTime = new HecTime("20Jan2010", "2400");
        tsc.startTime = hecTime.value();
        tsc.fullName = "/Basin/Location/Flow//1Hour/Quality/";
        tsc.units = "CFS";
        tsc.type = "Inst-Val";


             
               
        System.out.println("##JAVA## storing data...");
        status = dssTimeSeries.write(tsc);
        System.out.println("##JAVA ##stored data...");
        dssTimeSeries.done();           
        if (status != 0) return;

        HecDataManager.closeAllFiles();
        
        
		int numberValues = 3;
		tsc = new TimeSeriesContainer();
		HecTimeArray hecTimearray = new HecTimeArray(3);
		double values[] = new double[numberValues];
		for (int i=0; i<numberValues; i++) {
			values[i] = (double)(i + 10);
		}		
		
		hecTimearray.element(0).set("01jan-10000 1300");
		hecTimearray.element(1).set("01feb-10000 1500");
		hecTimearray.element(2).set("24mar-10000 2000");
		

		String pathname = "/aa7/b/c//IR-Century/year-10000/";
		tsc.setName(pathname);
		tsc.setUnits("CFS");
		tsc.setType("Inst-Val");
		tsc.set(values, hecTimearray);

		

		 dssTimeSeries = new HecTimeSeries();
		//  Force the file open to create if it does not exist
		status = dssTimeSeries.open();
		if (status != 0) {
			System.out.println("Cannot open DSS file " + DSSFileName + ", status: " + status);
			return;
		}
		//  Must be version 7 (not supported in version 6)
		if (dssTimeSeries.getDssFileVersion() != 7) {
			System.out.println("DSS file is not version 7, " + DSSFileName);
			return;		
		}
		HecDataManager.setMessageLevel(15);
		status = dssTimeSeries.write(tsc);
		dssTimeSeries.done();		
		if (status != 0) {
			System.out.println("Error in write. DSS file " + DSSFileName + ", status: " + status);
			System.out.println("Pathname: " + pathname);
			HecDataManager.closeAllFiles();
			HecDataManager.closeMessageFile();
			return;
		}
		else if (status == 0){
			return;
		}
		System.out.println("Wrote dataset.  Pathname: " + pathname);
		tsc = null;
		dssTimeSeries = null;
		values = null;

		//  Read data
		TimeSeriesContainer tscRead = new TimeSeriesContainer();		
		tscRead.setName(pathname);		
		tscRead.retrieveAllTimes = true;
		HecTimeSeries dssTimeSeriesRead = new HecTimeSeries();	
		status = dssTimeSeriesRead.read(tscRead, true);
		dssTimeSeriesRead.done();		
		if (status != 0) {
			System.out.println("Error in read. DSS file " + DSSFileName + ", status: " + status);
			System.out.println("Pathname: " + pathname);
			HecDataManager.closeAllFiles();
			HecDataManager.closeMessageFile();
			return;
		}
		System.out.println("Read dataset.  Pathname: " + pathname);

		for (int i=0; i<tscRead.numberValues; i++) {
			hecTime.set(tscRead.times[i]);
			if (tscRead.julianBaseDate != 0) {
				hecTime.addDays(tscRead.julianBaseDate);
			}
			System.out.println("Ordinate: " + i + ", time: " + hecTime.dateAndTime() + 
					", value: " + tscRead.values[i]);
		}

		HecDataManager.closeAllFiles();
		HecDataManager.closeMessageFile();
		
		System.out.println("\nExample completed.\n");
	}

}
