import java.util.Vector;
import javax.swing.UIManager;
import hec.dataTable.HecDataTableFrame;
import hec.gfx2d.G2dDialog;
import hec.heclib.dss.*;
import hec.io.DataContainer;
import hec.io.PairedDataContainer;

//  Write a family of curves by pre-allocating space, then writing one curve at a time
//
public class ExamplePairedData3 {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}

		//  Use the same DSS file for all reads and writes
		HecDataManager.setDefaultDSSFileName("C:/temp/Example7.dss");		
		DSSPathname path = new DSSPathname("/Basin/Location/Stage-Damage///Test/");

		//  Gen up the data
		PairedDataContainer pdc1 = new PairedDataContainer();
		pdc1.setName(path.pathname());
		//  Pre-allocate space for 500 rows for a family of 10 curves
		//  Gen up the ordinates
		double xOrdinates[] = new double[500];
		for (int i=0; i<xOrdinates.length; i++) {
			xOrdinates[i] = (double)(i + 1) * 2;
		}	
		//  Set the write to allocate space
		pdc1.allocateSpace(xOrdinates, 10);
		pdc1.setXUnits("Feet");
		pdc1.setXType("Linear");
		pdc1.setYUnits("Dollars");
		pdc1.setYType("Linear");
		pdc1.setStoreAsDoubles(true);
						
		HecPairedData dssPairedData1 = new HecPairedData();
		int status = dssPairedData1.write(pdc1);
		dssPairedData1.done();		
		if (status != 0) return;
		
		//  Now write each of the 10 curves separately 
		for (int j=0; j<10; j++) {
			PairedDataContainer pdc = new PairedDataContainer();
			pdc.setName(path.pathname());
			double yOrdinates[][] = new double[1][500];
			for (int i=0; i<500; i++) {
				yOrdinates[0][i] = ((double)(i + 1) * 100.0) + (double)((j + 1) * 3000);
			}
			//  First curve is #1, not #0 (by convention)
			pdc.setStartingEndingCurve(j+1, j+1);
			pdc.setYOrdinates(yOrdinates);
			HecPairedData dssPairedData = new HecPairedData();
			status = dssPairedData.write(pdc);
			dssPairedData.done();		
			if (status != 0) return;
		}

		//  Read curve 4 and 5 and tabulate
		PairedDataContainer pdc3 = new PairedDataContainer();	
		pdc3.setName(path.pathname());
		pdc3.setStartingEndingCurve(4, 5);
		HecPairedData dssPairedData3 = new HecPairedData();
		status = dssPairedData3.read(pdc3);		
		dssPairedData3.done();		
		if (status != 0) return;
		//  Show in a table
		HecDataTableFrame table3 = new HecDataTableFrame(null);
		table3.setData(pdc3);
		table3.setVisible(true);

		//  Read all curves and plot
		PairedDataContainer pdc4 = new PairedDataContainer();	
		pdc4.setName(path.pathname());
		HecPairedData dssPairedData4 = new HecPairedData();
		status = dssPairedData4.read(pdc4);
		dssPairedData4.done();		
		if (status != 0) return;
		//  Show in a plot
		Vector<DataContainer> v = new Vector<DataContainer>();
		v.add(pdc4);
		hec.gfx2d.G2dDialog plot =  new G2dDialog(null, "My Plot", false, v);
		plot.setVisible(true);

		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
