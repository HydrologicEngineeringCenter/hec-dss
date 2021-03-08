import hec.heclib.dss.*;


//  A simple snipit of code to demonstrate catalog functions
//
public class ExampleCatalog2{

	public static void main (String args[])  {
		
		String fileName = "C:/temp/Sample7.dss";
		boolean fileExists = HecDataManager.doesDSSFileExist(fileName);
		if (!fileExists) {
			System.out.println("This test requires a DSS file: " + fileName);
			return;
		}
		
		//  You can use either HecDssCatalog or HecDataManager,
		//  which inherits from HecDssCatalog 
		HecDataManager dataManager = new HecDataManager();
		int status = dataManager.setDSSFileName(fileName);
		if (status != 0) {
			System.out.println("Cannot access DSS file: " + fileName);
			return;
		}
	
		//  Generic full sorted catalog
		//  public String[] getCatalog(boolean sorted, String pathWithWildChars);
		String[] pathnames = dataManager.getCatalog(true, null);
		if (pathnames == null) {
			System.out.println("No records in file: " + fileName);
			dataManager.done();
			return;
		}
		System.out.println("There are " + pathnames.length + " pathnames in the list, " +
				dataManager.numberRecords() + " records in DSS file " + fileName);
		int n = 5;
		if (pathnames.length < n) n = pathnames.length;
		System.out.println("First " + n + " pathnames are:");
		for (int j=0; j<n; j++) {
			System.out.println(pathnames[j]);
		}
		
		//  Now get all pathnames with an F part of "obs" and a C part that contains "flow"
		String pathWithWildChars = "/*/*/*flow*/*/*/obs/";
		pathnames = dataManager.getCatalog(true, pathWithWildChars);
		if (pathnames == null) {
			System.out.println("No records with wild chars " + pathWithWildChars + 
					" in file: " + fileName);
			dataManager.done();
			return;
		}
		System.out.println("There are " + pathnames.length + 
				" pathnames in the list that match the wild characters " +
				pathWithWildChars);
		n = 5;
		if (pathnames.length < n) n = pathnames.length;
		System.out.println("First " + n + " pathnames are:");
		for (int j=0; j<n; j++) {
			System.out.println(pathnames[j]);
		}

		//  Now a condensed catalog for observed Sacramento data
		pathWithWildChars = "/*/Sacramento/*/*/*/obs/";
		CondensedReference[] condensed = dataManager.getCondensedCatalog(pathWithWildChars);
		if (condensed == null) {
			System.out.println("No records with wild chars " + pathWithWildChars + 
					" in file: " + fileName);
			dataManager.done();
			return;
		}
		System.out.println("There are " + condensed.length + 
				" condensed pathnames in the list that match the wild characters " +
				pathWithWildChars);
		n = 5;
		if (condensed.length < n) n = condensed.length;
		System.out.println("First " + n + " condensed pathnames are:");
		for (int j=0; j<n; j++) {
			System.out.println(condensed[j]);  //  This prints the condensed pathname
		}

		//  Write the list of all pathnames to a file
		//  Note - If you have a large DSS file, this will be the best way
		//  to get all pathnames, prvoided you do not sort.
		String catalogName = dataManager.getCatalogName(dataManager.ABBREVIATED_CATALOG);  //  or
		//  String catalogName = File.createTempFile("Sample", null);
		//  public int catalogFile(String catalogName, boolean sorted, String pathWithWildChars);
		int numberPaths = dataManager.catalogFile(catalogName, false, null);
		System.out.println(numberPaths + " pathnames were written to file " + catalogName);
		//  DSS-7 just writes a list, while DSS-6 writes a title and number 
		//  for each path (depending on method).
		//  readCatalog will read either
		String paths[] = dataManager.readCatalog();
		
		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
