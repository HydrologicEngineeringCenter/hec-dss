import java.util.Vector;

import javax.swing.UIManager;

import hec.dataTable.HecDataTableFrame;
import hec.gfx2d.G2dDialog;
import hec.heclib.dss.*;
import hec.io.DataContainer;
import hec.io.TimeSeriesContainer;

//  A time pattern example for a unit hydrograph with 
//  irregular-interval time series data
//
public class ExampleTimePattern2 {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}
		
		//  Write a irregular interval data set.  Gen up the data and times
		//  Note - this could also be stored with a "1Hour" E part
		double values[] = new double[200];
		long times[] = new long[200];
		for (int i=0; i<100; i++) {
			values[i] = (double)i * 5;
			times[i] = (i + 1) * 60;
		}
		for (int i=100; i<200; i++) {
			values[i] = 500.0 + (double)(100 - i) * 5.0;
			times[i] = (i + 1) * 60;
		}
		TimeSeriesContainer tsc = new TimeSeriesContainer();
		//  No absolute times are specified, just relative
		tsc.set(values, times);
		tsc.setName("/Basin/Location/Flow/TS-Pattern/Ir-Month/Unit Hydrograph/");		
		tsc.setUnits("CFS");
		tsc.setType("Inst-Val");
		
		HecTimeSeries dssTimeSeries = new HecTimeSeries();
		dssTimeSeries.setDSSFileName("C:/temp/Example7.dss");		
		int status = dssTimeSeries.write(tsc);
		dssTimeSeries.done();		
		if (status != 0) return;
		
		//  Read data
		TimeSeriesContainer tscRead = new TimeSeriesContainer();		
		tscRead.setName("/Basin/Location/Flow/TS-Pattern/Ir-Month/Unit Hydrograph/");
		HecTimeSeries dssTimeSeriesRead = new HecTimeSeries();
		dssTimeSeriesRead.setDSSFileName("C:/temp/Example7.dss");		
		status = dssTimeSeriesRead.read(tscRead, false);
		dssTimeSeriesRead.done();		
		if (status != 0) return;

		//  Show in a plot
		Vector<DataContainer> v = new Vector<DataContainer>();
		v.add(tscRead);
		hec.gfx2d.G2dDialog plot =  new G2dDialog(null, "Unit Hydrograph", false, v);
		plot.setVisible(true);

		//  Show in a table
		HecDataTableFrame tableFrame = new HecDataTableFrame(null);
		tableFrame.setData(tscRead);
		tableFrame.setVisible(true);

		HecDataManager.closeAllFiles();
	}
}
