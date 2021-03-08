import hec.heclib.dss.*;
import hec.io.TimeSeriesContainer;

//  An example of using aliases
//  Assumes that sample7.dss contains the data from sample.dss 
//
public class ExampleAliases {

	public static void main (String args[])  {
	
		HecDataManager dataManager = new HecDataManager();
		int status = dataManager.setDSSFileName("C:/temp/Sample7.dss");
		if (status != 0) {
			System.out.println("Cannot access DSS file: C:/temp/Sample7.dss" );
			return;
		}
		String primary = "/AMERICAN/FOLSOM/FLOW-RES IN/01Jan2006/1Day/OBS/";
		if ((dataManager.numberRecords() < 100) || (dataManager.recordExists(primary) == false)) {
			System.out.println("This example assumes that sample7.dss contains same data as sample.dss");
			return;
		}
		//  Add an alias to this path
		DSSPathname alias = new DSSPathname(primary);
		alias.setFPart("Alias Testing");
		status = dataManager.addAlias(primary, alias.pathname());
		if (status < 0) return;
		if (dataManager.recordExists(alias.pathname())) System.out.println("Alias checks: " + alias.pathname());
		// Add a second - doesn't matter if you link it to primary or to alias
		alias.setFPart("Testing Second Time");
		status = dataManager.addAlias(primary, alias.pathname());
		if (status < 0) return;
		if (dataManager.recordExists(alias.pathname())) System.out.println("Alias checks: " + alias.pathname());
		
		//  Get a list of the aliases that are linked to the priamry. 
		//  Since an alias points to the primary, you can use either an alias or priamry to get the list
		String paths[] = dataManager.aliasList(alias.pathname());
		//  First one in the list is always the primary
		if (paths.length > 0) System.out.println("Primary: " + paths[0]);
		for (int i=1; i<paths.length; i++) {
			System.out.println("Alias " + i + " is " + paths[i]);
		}
		
		//  Now remove an alias - don't delete!  Delete will remove both aliases, primary and data
		boolean removeAll = false;
		status = dataManager.aliasRemove(alias.pathname(), removeAll);
		if (dataManager.recordExists(alias.pathname()) == false) System.out.println("Alias does not exist: " + alias.pathname());
		
		//  Suppose you had a time series data set (with many records)?
		//  You can have a condensed pathname or a time series container, for example...
		TimeSeriesContainer tsc = new TimeSeriesContainer();		
		tsc.fullName = "//SACRAMENTO/TEMP-MAX/01JAN1980/1DAY/OBS/";		
		HecTimeSeries dssTimeSeries = new HecTimeSeries();
		dssTimeSeries.setDSSFileName("C:/temp/Sample7.dss");
		tsc.retrieveAllTimes = true;  //  Same as dssTimeSeries.setRetrieveAllTimes(true);
		status = dssTimeSeries.read(tsc, true);
		dssTimeSeries.done();	
		if (status != 0) return;
		//  Use a condensed reference to get the full list of pathnames in the set
		CondensedReference cr = new CondensedReference(tsc);  //  Could also give it a path with a date range
		paths = cr.getPathnameList();  // just to show how to get the list
		DSSPathname aliasPart = new DSSPathname();
		aliasPart.setBPart("New Location");
		status = dataManager.addAlias(cr, aliasPart);
		
		dataManager.done();
		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
