import java.util.Vector;

import javax.swing.UIManager;

import hec.heclib.dss.*;


//  A simple snipit of code to demonstrate some DSS utility functions
//
public class ExampleUtilities {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}
		
		int version = 0;
		String fileName = "";
		boolean fileExists;
		for (int i=6; i<8; i++) {
			if (i == 6) {
				fileName = "C:/temp/Sample.dss";
			}
			else {
				fileName = "C:/temp/Sample7.dss";
			}
			fileExists = HecDataManager.doesDSSFileExist(fileName);
			if (fileExists) {				
				version = i;
				HecDSSUtilities dm = new HecDSSUtilities();
				dm.setDSSFileName(fileName);				
				String paths[] = dm.getPathnameList(false);
				if (paths == null) {
					System.out.println("No records in file: " + fileName);
					continue;
				}
				System.out.println("There are " + paths.length + " pathnames in the list, " +
						dm.numberRecords() + " records in DSS file " + fileName);
				int n = 5;
				if (paths.length < n) n = paths.length;
				System.out.println("First " + n + " pathnames are:");
				for (int j=0; j<n; j++) {
					System.out.println(paths[j]);
				}
				String path = paths[n-1];
				Vector<hec.heclib.dss.CondensedReference> cd = dm.getCondensedCatalog();
				n = 5;
				if (cd.size() < n) n = cd.size();
				System.out.println("First " + n + " condensed pathnames are:");
				for (int j=0; j<n; j++) {					
					System.out.println(cd.get(j).getNominalPathname());
				}
				DSSPathname dpath = new DSSPathname(path);
				System.out.println(dpath.pathname() + " exists: " + dm.recordExists(dpath.pathname()));
				Vector<String> v1 = new Vector(1);
				v1.add(dpath.pathname());
				dm.delete(v1);
				System.out.println(dpath.pathname() + " exists: " + dm.recordExists(dpath.pathname()));
				dm.undelete(v1);
				System.out.println(dpath.pathname() + " exists: " + dm.recordExists(dpath.pathname()));
				dpath.setAPart("aaaaa");
				Vector<String> v2 = new Vector(1);
				v2.add(dpath.pathname());
				dm.renameRecords(v1, v2);
				System.out.println(v1.get(0) + " exists: " + dm.recordExists(v1.get(0)));
				System.out.println(v2.get(0) + " exists: " + dm.recordExists(v2.get(0)));
				dm.delete(v2);
				dm.squeeze();
				int status = dm.undelete(v2);
				System.out.println("Undelete status = " + status);
				System.out.println(v2.get(0) + " exists: " + dm.recordExists(v2.get(0)));					
			}
		}
		if (version == 0) {
			System.out.println("This test requires a DSS file: " + fileName);
			return;
		}		
		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
