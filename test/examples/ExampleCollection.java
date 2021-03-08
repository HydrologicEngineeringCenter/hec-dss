import java.util.Arrays;
import java.util.Vector;

import hec.gfx2d.G2dDialog;
import hec.heclib.dss.*;
import hec.heclib.util.HecTime;
import hec.hecmath.TimeSeriesMath;
import hec.io.TimeSeriesCollectionContainer;
import hec.io.TimeSeriesContainer;

//  Example time series collection
//
public class ExampleCollection {

	public static void main (String args[])  {

		//  Create a collection from daily precip for POR for Sacramento in sample DSS file
		HecDataManager.setDefaultDSSFileName("C:/temp/Sample7.dss");
		String pathname = "//SACRAMENTO/PRECIP-INC/*/1DAY/OBS/";
		HecDssCatalog catalog = new HecDssCatalog();
		String paths[] = catalog.getCatalog(true, pathname);
		if ((paths == null)  || (paths.length < 2)) {
			System.out.println("Incorrect DSS file - please copy HEC-DSSVue sample.dss to C:/temp/Sample7.dss");
			return;
		}
		
		TimeSeriesCollectionContainer tscc = new TimeSeriesCollectionContainer();
		for (int i=0; i<paths.length; i++) {
			HecTimeSeries timeSeries = new HecTimeSeries();
			TimeSeriesContainer tsc = new TimeSeriesContainer();
			tsc.setName(paths[i]);
			int status = timeSeries.read(tsc, true);
			if (status < 0) {
				System.out.println("Error reading " + tsc.getName() + ", status: " + status);
				return;
			}
			//  For simplicty, only use years with 365 days.
			if (tsc.getNumberValues() == 365) {
				//  Accumulate precip
				try {
					TimeSeriesMath math = new TimeSeriesMath();
					math.setData(tsc);
					math = (TimeSeriesMath)math.accumulation();
					math = (TimeSeriesMath)math.shiftInTime(new HecTime("01Jan3000, 2400"));
					tsc = math.getContainer();
				}
				catch (Exception e) {
					System.out.println(e);
					return;
				}
				//  Store collection for year 3000 (usual when doing statistical)
				//  (Could also use HecMath shiftInTime)
				tsc.setTimes(null);
				tsc.setStartTime(new HecTime("01Jan3000, 2400"));
				DSSPathname path = new DSSPathname(paths[i]);
				path.setDPart("01Jan3000");
				path.setCollectionSequence(i);
				tsc.setName(path.pathname());
				tscc.add(tsc);
			}
		}
		//  finish causes collections calculations to be run
		tscc.finishedAdding();
		
		if (tscc.numberOfSequences() > 0) {
			HecTimeSeries timeSeries1 = new HecTimeSeries();
			int status = timeSeries1.write(tscc);
			if (status < 0) {
				System.out.println("Error writting " + tscc.getName() + ", status: " + status);
				return;
			}
			else {
				System.out.println("Collection container " + tscc.getName() + " written.");  
				System.out.println("Number sequences: " + tscc.numberOfSequences());
			}
		}
		
		//  Read collection, then plot
		HecTimeSeries timeSeries2 = new HecTimeSeries();
		TimeSeriesCollectionContainer tscc2 = new TimeSeriesCollectionContainer();
		tscc2.setName(tscc.getName());
		int status = timeSeries2.read(tscc2, true);
		if (status < 0) {
			System.out.println("Error reading " + tscc2.getName() + ", status: " + status);
			return;
		}

		//  Show in a plot
		Vector<TimeSeriesContainer> v = new Vector<TimeSeriesContainer>(Arrays.asList(tscc2.get()));
		hec.gfx2d.G2dDialog plot =  new G2dDialog(null, "My Plot", false, v);
		plot.setVisible(true);

		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
