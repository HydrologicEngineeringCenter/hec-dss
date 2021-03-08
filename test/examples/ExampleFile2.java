import java.io.File;
import hec.heclib.dss.*;

//  An example of storing a file in DSS, then retrieving and running with the default program
//
public class ExampleFile2 {

	public static void main (String args[])  {
	
		try {
			HecDssFileStore fileStore = new HecDssFileStore();
			int status = fileStore.setDSSFileName("C:/temp/Sample7.dss");
			if (status != 0) {
				System.out.println("Cannot access DSS file: C:/temp/Sample7.dss" );
				return;
			}
			//  Use the sample video included with Windows
			//  You can do this with most any file, though
			File file = new File("C:/Users/Public/Videos/Sample Videos/Wildlife.wmv");
			DSSPathname pathname = new DSSPathname();
			//  Set the C part to the file name, D part to "FILE" and E part to the extension
			pathname.setCPart("Wildlife.wmv");
			pathname.setDPart("FILE");
			pathname.setEPart("wmv");
			System.out.println("Pathname: " + pathname.toString());
			//  Write to DSS
			status = fileStore.write(file, pathname);
			if (status != 0) {
				System.out.println("Error storing " + pathname);
				return;
			}

			//  Now read the file from DSS, store in a temporary file and then play it.
			//  This shows what is going on in "runProcess()"
			//  We'll use the same fileStore object for convenience 
			//  Original file name without extension
			String name = pathname.cPart().substring(0, pathname.cPart().lastIndexOf("."));		
			File tempFile = File.createTempFile(name, "." + pathname.ePart());
			tempFile.deleteOnExit();
			//  Read from DSS and write to temp file
			fileStore.setPathname(pathname.toString());
			status = fileStore.exportToFile(tempFile);
			if (status != 0) {
				System.out.println("Error retrieving " + pathname);
				return;
			}
			fileStore.done();
			//  Now run it using exec:rundll32 url.dll,FileProtocolHandler filename
			hec.util.Util.runProcess(tempFile.getAbsolutePath());
			//  Give the process some time to get started, otherwise this sample will
			//  exit before it does (a regular program does not need to do this!)
			Thread.sleep(5000);
		}
		catch(Exception e) {
			System.out.println("Exception: " + e);
		}
		
		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
