import java.util.Vector;
import javax.swing.UIManager;

import hec.dataTable.HecDataTableFrame;
import hec.gfx2d.G2dDialog;
import hec.heclib.dss.*;
import hec.io.DataContainer;
import hec.io.TimeSeriesContainer;

//  Create a time pattern (from an existing data set)

public class ExampleTimePattern1 {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}
		
		//  First, get a data set that we can use
		TimeSeriesContainer tsc = new TimeSeriesContainer();		
		tsc.setName("//Sacramento/TEMP-MAX/01JAN1980/1Day/Obs/");
		HecTimeSeries dssTimeSeries = new HecTimeSeries();
		//  This is the "sample.dss" file included with HEC-DSSVue, converted to version 7
		dssTimeSeries.setDSSFileName("C:/temp/Sample7.dss");
		int status = dssTimeSeries.read(tsc, true);
		dssTimeSeries.done();		
		if (status != 0) return;
		
		//  Now save it as a time pattern
		TimeSeriesContainer tsc2 = new TimeSeriesContainer();		
		tsc2.setName("//Sacramento/TEMP-MAX/TS-Pattern/1Day/Daily Average/");
		tsc2.setValues(tsc.getValues());
		tsc2.setUnits("Deg F");
		tsc2.setType("Per-Aver");
		//  Note - no starting or ending times set!  (Just data)
		dssTimeSeries = new HecTimeSeries();
		dssTimeSeries.setDSSFileName("C:/temp/Example7.dss");
		status = dssTimeSeries.write(tsc2);
		
		//  Now read it back and display
		TimeSeriesContainer tsc3 = new TimeSeriesContainer();		
		tsc3.setName("//Sacramento/TEMP-MAX/TS-Pattern/1Day/Daily Average/");
		dssTimeSeries = new HecTimeSeries();
		dssTimeSeries.setDSSFileName("C:/temp/Example7.dss");
		status = dssTimeSeries.read(tsc3, true);
		dssTimeSeries.done();		
		if (status != 0) return;

		//  Show in a plot
		Vector<DataContainer> v = new Vector<DataContainer>();
		v.add(tsc3);
		hec.gfx2d.G2dDialog plot =  new G2dDialog(null, "My Plot", false, v);
		plot.setVisible(true);

		//  Show in a table
		 HecDataTableFrame tableFrame = new HecDataTableFrame(null);
		 tableFrame.setData(tsc3);
		 tableFrame.setVisible(true);

		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
