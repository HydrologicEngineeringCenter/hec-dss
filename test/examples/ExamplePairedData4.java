import javax.swing.UIManager;
import hec.dataTable.HecDataTableFrame;
import hec.heclib.dss.*;
import hec.io.PairedDataContainer;

//  Read and write a block of a paired data record
//
public class ExamplePairedData4 {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}

		//  Use the same DSS file for all reads and writes
		HecDataManager.setDefaultDSSFileName("C:/temp/Example7.dss");		
		DSSPathname path = new DSSPathname("/Basin/Location/Stage-Damage///Test 4/");

		//  Gen up a full dataset
		PairedDataContainer pdc1 = new PairedDataContainer();	
		pdc1.setName(path.pathname());
		//  write a data set for 500 rows for a family of 10 curves
		//  Gen up the ordinates
		double xOrdinates[] = new double[500];
		double yOrdinates[][] = new double[10][500];
		for (int i=0; i<xOrdinates.length; i++) {
			xOrdinates[i] = (double)(i + 1) * 2;
			for (int j=0; j<10; j++) {
				yOrdinates[j][i] = ((double)(i + 1) * 100.0) + (double)((j + 1) * 3000);
			}	
		}
		//  Set the arrays
		pdc1.setValues(xOrdinates, yOrdinates);
		pdc1.setXUnits("Feet");
		pdc1.setXType("Linear");
		pdc1.setYUnits("Dollars");
		pdc1.setYType("Linear");
		HecPairedData dssPairedData1 = new HecPairedData();
		int status = dssPairedData1.write(pdc1);
		dssPairedData1.done();		
		if (status != 0) return;

		//  Read a block and tabulate
		PairedDataContainer pdc3 = new PairedDataContainer();	
		pdc3.setName(path.pathname());
		pdc3.setStartingEndingCurve(5, 9);
		pdc3.setStartingEndingOrdinage(2, 5);
		HecPairedData dssPairedData3 = new HecPairedData();
		status = dssPairedData3.read(pdc3);			
		if (status != 0) return;
		//  Show in a table
		HecDataTableFrame table3 = new HecDataTableFrame(null);
		table3.setData(pdc3);
		table3.setVisible(true);

		//  Change the numbers to negative and store
		double xords[] = pdc3.getXOridnates();
		double yords[][] = pdc3.getYOridnates();
		for (int i=0; i<xords.length; i++) {
			for (int j=0; j<yords[0].length; j++) {
				yords[j][i] = -yords[j][i];
			}
		}	
		//  pdc3 retains the starting / ending curves and ordinates
		pdc3.setXOrdinates(xords);
		pdc3.setYOrdinates(yords);
		status = dssPairedData3.write(pdc3);		
		dssPairedData3.done();		
		if (status != 0) return;	

		//  Read all curves and show in table
		PairedDataContainer pdc4 = new PairedDataContainer();	
		pdc4.setName(path.pathname());
		HecPairedData dssPairedData4 = new HecPairedData();
		status = dssPairedData4.read(pdc4);
		dssPairedData4.done();		
		if (status != 0) return;
		//  Show in a plot
		HecDataTableFrame table4 = new HecDataTableFrame(null);
		table4.setData(pdc4);
		table4.setVisible(true);

		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
