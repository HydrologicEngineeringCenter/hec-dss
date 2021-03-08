import javax.swing.UIManager;
import hec.dataTable.HecDataTableFrame;
import hec.heclib.dss.*;
import hec.io.PairedDataContainer;


//  A simple sample of code to demonstrate storing and retrieving 
//  paired data for a rating table
//
public class ExamplePairedData1 {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}
		
		//  Gen up the data
		//  double xOrdinates[int numberOrdinates]		
		double xOrdinates[] = new double[200];
		//  double yOrdinates[int numberCurves][int numberOrdinates]
		double yOrdinates[][] = new double[1][200];
		//  Gen up data to store
		for (int i=0; i<200; i++) {
			xOrdinates[i] = (double)(i + 1);
			yOrdinates[0][i] = (double)(i + 1) * 100.0;
		}
		PairedDataContainer pdc1 = new PairedDataContainer();	
		pdc1.setName("/Basin/Location/Stage-Flow/Rating Table/Java Sample//");
		pdc1.setValues(xOrdinates, yOrdinates);
		pdc1.setXUnits("Feet");
		pdc1.setXType("Linear");
		pdc1.setYUnits("CFS");
		pdc1.setYType("Linear");
		
		HecPairedData dssPairedData1 = new HecPairedData();
		dssPairedData1.setDefaultDSSFileName("C:/temp/Example7yyy.dss");
		//  We could have set with setDefaultDSSFileName for the entire example
		int status = dssPairedData1.write(pdc1);
		dssPairedData1.done();		
		if (status != 0) return;
				
		//  Read the data
		PairedDataContainer pdc2 = new PairedDataContainer();	
		pdc2.setName("/Basin/Location/Stage-Flow/Rating Table/Java Sample//");
		HecPairedData dssPairedData2 = new HecPairedData();
		dssPairedData2.setDSSFileName("C:/temp/Example7.dss");
		status = dssPairedData2.read(pdc2);		
		dssPairedData2.done();		
		if (status != 0) return;
		
	//  Show in a table
		HecDataTableFrame table = new HecDataTableFrame(null);
		table.setData(pdc2);
		table.setVisible(true);

		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
