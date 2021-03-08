import javax.swing.UIManager;
import hec.heclib.dss.*;
import hec.io.PairedDataContainer;


//  A rating table showing the use of location information
//
/*
 * USGS 11464000 RUSSIAN R NR HEALDSBURG CA
 * Latitude 38°36'48",   Longitude 122°50'07"   NAD27
 * Sonoma County, California, Hydrologic Unit 18010110
 * Drainage area: 793 square miles
 * Datum of gage: 77.01 feet above   NGVD29.
 */
public class ExampleLocation {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}
		
		//  Gen up the data
		PairedDataContainer pdc1 = new PairedDataContainer();	
		pdc1.fullName = "/Russian/Healdsburg/Stage-Flow//NWIS RATING//";		
		pdc1.xOrdinates = new double[200];
		pdc1.numberOrdinates = 200;
		pdc1.yOrdinates = new double[1][200];
		pdc1.numberCurves = 1;
		//  Gen up data to store
		for (int i=0; i<pdc1.numberOrdinates; i++) {
			pdc1.xOrdinates[i] = (double)(i + 1);
			pdc1.yOrdinates[0][i] = (double)(i + 1) * 100.0;
		}
		pdc1.xunits = "Feet";
		pdc1.xtype = "Linear";
		pdc1.yunits = "CFS";
		pdc1.ytype = "logarithmic";
		
		pdc1.xOrdinate = -122.8353;	//	Longitude, negative for western hemp 
		pdc1.yOrdinate = 38.6133;	//  Latitude
		pdc1.coordinateSystem = 1; 	//  Lat/Long
		pdc1.horizontalUnits = 3;	//  Decimal Degrees 
		pdc1.horizontalDatum = 2;	//  NAD27
		pdc1.zOrdinate = 77.01;		//  Elevation
		pdc1.verticalUnits = 1;		//  Feet
		pdc1.verticalDatum = 2;		//  NGVD29
		pdc1.locationTimezone = "America/Los Angeles";  
		pdc1.supplementalInfo =	"USGS 11464000 RUSSIAN R NR HEALDSBURG CA\n" +
								"Sonoma County, California, Hydrologic Unit 18010110\n" +
								"Drainage area: 793 square miles\n" + 
								"SHIFTED=20161214140000 PST\n" +
								"BREAKPOINT1=1.74, OFFSET1=0.20, OFFSET2=0.30\n" +
								"STAGE1=4.33, SHIFT1=0.23, STAGE2=8.00, SHIFT2=0.00\n" +
								"Rating 50 has been developed in response to the\n" + 
								"scour of the channel found during WY2015.\n" +
								"Comes into effect on the peak of 12/11/14.";
								
		
		HecPairedData dssPairedData1 = new HecPairedData();
		dssPairedData1.setDSSFileName("C:/temp/Sample7.dss");
		int status = dssPairedData1.write(pdc1);		
		dssPairedData1.done();	
		//  Or, you could do this if you did not have data to store with the location info
		//HecDataManager locData1 = new HecDataManager();
		//locData1.setDSSFileName("C:/temp/Sample7.dss");
		//status = locData1.zlocationStore (pdc1);

		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
