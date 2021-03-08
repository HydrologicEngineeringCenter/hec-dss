import java.util.Vector;
import javax.swing.UIManager;
import hec.gfx2d.G2dDialog;
import hec.heclib.dss.*;
import hec.io.DataContainer;
import hec.io.PairedDataContainer;


//  A simple sample of code to demonstrate storing and retrieving a flow-frequency curve set
//
public class ExamplePairedData2 {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}
		
		double xOrdinates[] = new double[20];
		//  numberOrdinates = 20;
		double yOrdinates[][] = new double[4][20];
		//  numberCurves = 4;
		//  Gen up data to store
		for (int i=0; i<xOrdinates.length; i++) {
			xOrdinates[i] = (double)(i + 1) / (double)(xOrdinates.length + 1);
		}
		for (int i=0; i<4; i++) {
			for (int j=0; j<xOrdinates.length; j++) {
				yOrdinates[i][j] = (double)(i + 1) * 10.0 + (double)(j * 5);
			}
		}		

		PairedDataContainer pdc1 = new PairedDataContainer();	
		pdc1.setName("/Basin/Location/Freqency-Flow/Java Sample///");
		pdc1.setValues(xOrdinates, yOrdinates);
		pdc1.setXUnits("Percent");
		pdc1.setXType("Freqency");
		pdc1.setYUnits("CFS");
		pdc1.setYType("Linear");
		
		String labels[] = new String[4];
		labels[0] = "Plan A"; 
		labels[1] = "Plan B"; 
		labels[2] = "Plan C"; 
		labels[3] = "No Changes"; 
		pdc1.setLabels(labels);
		
		HecPairedData dssPairedData1 = new HecPairedData();
		dssPairedData1.setDSSFileName("C:/temp/Example7.dss");
		int status = dssPairedData1.write(pdc1);
		dssPairedData1.done();		
		if (status != 0) return;
		
		//  Read the data
		PairedDataContainer pdc2 = new PairedDataContainer();	
		pdc2.setName("/Basin/Location/Freqency-Flow/Java Sample///");
		HecPairedData dssPairedData2 = new HecPairedData();
		dssPairedData2.setDSSFileName("C:/temp/Example7.dss");
		status = dssPairedData2.read(pdc2);
		dssPairedData2.done();		
		if (status != 0) return;
		//  Show in a plot
		Vector<DataContainer> v = new Vector<DataContainer>();
		v.add(pdc2);
		hec.gfx2d.G2dDialog plot =  new G2dDialog(null, "My Plot", false, v);
		plot.setVisible(true);

		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
