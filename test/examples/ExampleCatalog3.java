import hec.heclib.dss.*;
import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;

//  An example of determing what data in a DSS file has changed
//
public class ExampleCatalog3 {

	public static void main (String args[])  {
	
		HecDssCatalog catalog = new HecDssCatalog();
		int status = catalog.setDSSFileName("C:/temp/Sample7.dss");
		if (status != 0) {
			System.out.println("Cannot access DSS file: C:/temp/Sample7.dss" );
			return;
		}
		//  public int whatChangedSetStart(String pathnameWithWildChars, boolean useCRCforData)
		//  pathnameWithWildChars = "/*/*/Flow/*/*/Study 1/".  CRC checks data changes, not last write
		catalog.whatChangedSetStart(null, true);
		
		//  Do stuff, computes, etc.
		HecTimeSeries ts = new HecTimeSeries();
		ts.setDSSFileName("C:/temp/Sample7.dss");
		TimeSeriesContainer tsc = new TimeSeriesContainer();
		tsc.setName("//SACRAMENTO/TEMP-MAX/01Jan1900/1Day/OBS/");
		HecTime start = new HecTime("20June1950", "0001");
		tsc.setStartTime(start);
		HecTime end = new HecTime("01Oct1970", "2400");
		tsc.setEndTime(end);
		status = ts.read(tsc, true);
		if (status != 0) return;
		//  Multiply by 2
		double vals[] = tsc.getValues();
		for (int i=0; i<vals.length; i++) {
			vals[i] *= 2.0;
		}
		tsc.setValues(vals);
		status = ts.write(tsc);
		ts.done();

		String[] paths = catalog.whatChanged();
		if (paths.length > 0) {
			//  Sort the pathnames to look nice
			String[] pathnames = catalog.sort();
			catalog.done();
			if (pathnames != null) {
				System.out.println(pathnames.length + " pathnames changed.  They are: " );
				for (int j=0; j<pathnames.length; j++) {
					System.out.println(pathnames[j]);
				}
			}
		}  

		HecDssCatalog.closeAllFiles();  //  Only at the end of the program
	}
}
