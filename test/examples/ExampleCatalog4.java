import hec.heclib.dss.*;
import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;

//  An example of determing different kinds of datasets that have changed
//
public class ExampleCatalog4 {

	public static void main (String args[])  {
	
		HecDssCatalog catalog1 = new HecDssCatalog();
		int status = catalog1.setDSSFileName("C:/temp/Sample7.dss");
		if (status != 0) {
			System.out.println("Cannot access DSS file: C:/temp/Sample7.dss" );
			return;
		}
	
		//  public int whatChangedSetStart(String pathnameWithWildChars, boolean useCRCforData)
		//  pathnameWithWildChars = "/*/*/Flow/*/*/Study 1/".  CRC checks data changes, not last write
		catalog1.whatChangedSetStart("/*/*/Temp*/*/*/*/", true);
		HecDssCatalog catalog2 = new HecDssCatalog();
		status = catalog2.setDSSFileName("C:/temp/Sample7.dss");
		if (status < 0) return;
		catalog2.whatChangedSetStart("/*/*/Precip*/*/*/*/", true);
		
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
		tsc = new TimeSeriesContainer();
		tsc.setName("//SACRAMENTO/PRECIP-INC/01Jan1900/1Day/OBS/");
		tsc.setStartTime(start);
		tsc.setEndTime(end);
		status = ts.read(tsc, true);
		if (status != 0) return;
		//  Multiply by 5
		vals = tsc.getValues();
		for (int i=0; i<vals.length; i++) {
			vals[i] *= 5.0;
		}
		tsc.setValues(vals);
		status = ts.write(tsc);
		ts.done();

		String[] paths = catalog1.whatChanged();
		if (paths.length > 0) {
			//  Sort the pathnames to look nice
			String[] pathnames = catalog1.sort();
			catalog1.done();
			if (pathnames != null) {
				System.out.println(pathnames.length + " Temperature pathnames changed.  They are: " );
				for (int j=0; j<pathnames.length; j++) {
					System.out.println(pathnames[j]);
				}
			}
		}
		
		paths = catalog2.whatChanged();
		if (paths.length > 0) {
			//  Sort the pathnames to look nice
			String[] pathnames = catalog2.sort();
			catalog2.done();
			if (pathnames != null) {
				System.out.println(pathnames.length + " Precip pathnames changed.  They are: " );
				for (int j=0; j<pathnames.length; j++) {
					System.out.println(pathnames[j]);
				}
			}
		}

		HecDssCatalog.closeAllFiles();  //  Only at the end of the program
	}
}
