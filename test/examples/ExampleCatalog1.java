import hec.heclib.dss.*;

//  A short example of getting a catalog or list of pathnames in a DSS file
//
public class ExampleCatalog1 {

	public static void main (String args[])  {
	
		HecDataManager dataManager = new HecDataManager();
		int status = dataManager.setDSSFileName("C:/temp/Sample7.dss");
		if (status != 0) {
			System.out.println("Cannot access DSS file: C:/temp/Sample7.dss" );
			return;
		}
	//  public String[] getCatalog(boolean sorted, String pathWithWildChars);
		String[] pathnames = dataManager.getCatalog(true, null);
		dataManager.done();
		if (pathnames != null) {			
			int n = 5;
			if (pathnames.length < n) n = pathnames.length;
			System.out.println("First " + n + " pathnames are:");
			for (int j=0; j<n; j++) {
				System.out.println(pathnames[j]);
			}
		}
		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
