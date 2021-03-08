import java.util.Vector;
import javax.swing.UIManager;

import hec.dataTable.HecDataTableFrame;
import hec.gfx2d.G2dDialog;
import hec.heclib.dss.*;
import hec.io.DataContainer;
import hec.io.TimeSeriesContainer;

//  Get all data (period of record) for a dataset in a DSS file
//  We don't know the time window
public class ExampleTimeSeries3 {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}
		
		//  In the example DSS file from HEC-DSSVue, there is period of record 
		//  precip and temperature.  
		//  Get the max temperature for POR and plot it.
		//  We don't know when it stops or ends, so use POR flag
		TimeSeriesContainer tsc = new TimeSeriesContainer();		
		tsc.setName("//SACRAMENTO/TEMP-MAX/01JAN1980/1DAY/OBS/");
		tsc.retrieveAllTimes = true;
		HecTimeSeries dssTimeSeries = new HecTimeSeries();
		//  This is the "sample.dss" file included with HEC-DSSVue, converted to version 7
		dssTimeSeries.setDSSFileName("C:/temp/Sample7.dss");
		//  If you have issues, show user debug info
		//  dssTimeSeries.zsetMessageLevel(HecDataManager.MESS_METHOD_GLOBAL, 
		//									 HecDataManager.MESS_LEVEL_USER_DIAG);
		int status = dssTimeSeries.read(tsc, true);
		dssTimeSeries.done();		
		if (status != 0) return;

		//  Show in a plot
		Vector<DataContainer> v = new Vector<DataContainer>();
		v.add(tsc);
		hec.gfx2d.G2dDialog plot =  new G2dDialog(null, "My Plot", false, v);
		plot.setVisible(true);

		//  Show in a table
		 HecDataTableFrame tableFrame = new HecDataTableFrame(null);
		 tableFrame.setData(tsc);
		 tableFrame.setVisible(true);

		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
