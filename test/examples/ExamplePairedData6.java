import javax.swing.UIManager;
import hec.dataTable.HecDataTableFrame;
import hec.heclib.dss.*;
import hec.io.PairedDataContainer;

//  Read and write a block of a paired data record
//
public class ExamplePairedData6 {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}

		//  Use the same DSS file for all reads and writes
		HecDataManager.setDefaultDSSFileName("C:/temp/Example7.dss");		
		DSSPathname path = new DSSPathname("/Basin/Location/Stage-Damage///Test 6/");
		PairedDataContainer pdc = new PairedDataContainer();
		double xOrdinates[] = new double[500];		
		//  Set the write to allocate space, with 500 ordinances, 10 curves
		for (int i=0; i<xOrdinates.length; i++) {
			xOrdinates[i] = (double)(i + 1) * 2;
		}
		pdc.setName(path.pathname());
		pdc.allocateSpace(xOrdinates, 10);
		HecPairedData dssPairedData = new HecPairedData();
		//  Write ordinates and allocate space		
		int status = dssPairedData.write(pdc);


		//  Read a block and tabulate
		pdc = new PairedDataContainer();
		pdc.setName(path.pathname());
		//  Because the record already has been allocated and
		//  the start/end curve and ordinates are defined,
		//  numberOrdinates and numberCurves is not needed.		
		//  First curve is #1, not #0 (by convention)
		pdc.setStartingEndingCurve(4, 4);
		pdc.setStartingEndingOrdinage(50, 55);
		//  yOrdinates[numberCurves][numberOrdinates]
		double yOrdinates[][] = new double[1][1];
		yOrdinates[0][0] = 123.456;
		pdc.setYOrdinates(yOrdinates);
		HecPairedData dssPairedData3 = new HecPairedData();
		status = dssPairedData3.write(pdc);

			
		if (status != 0) return;
		//  Show in a table
		HecDataTableFrame table3 = new HecDataTableFrame(null);
		pdc = new PairedDataContainer();
		pdc.setName(path.pathname());
		HecPairedData dssPairedData4 = new HecPairedData();
		status = dssPairedData4.read(pdc);
		table3.setData(pdc);
		table3.setVisible(true);


		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
