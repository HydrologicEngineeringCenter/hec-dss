import java.io.File;
import hec.heclib.dss.*;


//  An example of storing an .mp3 file in DSS, then retrieving and playing with Media Player
//
public class ExampleFile1 {

	public static void main (String args[])  {
	
		try {
			HecDssFileStore fileStore = new HecDssFileStore();
			int status = fileStore.setDSSFileName("C:/temp/Sample7.dss");
			if (status != 0) {
				System.out.println("Cannot access DSS file: C:/temp/Sample7.dss" );
				return;
			}

			//  Use the sample music included with Windows
			File file = new File("C:/Users/Public/Music/Sample Music/Sleep Away.mp3");
			DSSPathname pathname = new DSSPathname();
			//  Set the C part to the file name, D part to "FILE" and E part to the extension (mp3)
			pathname.setCPart("Sleep Away.mp3");
			pathname.setDPart("FILE");
			pathname.setEPart("mp3");
			System.out.println("Pathname: " + pathname.toString());
			//  Write to DSS
			status = fileStore.write(file, pathname);
			if (status != 0) {
				System.out.println("Error storing " + pathname);
				return;
			}

			//  Now read the file from DSS, store in a temporary file and then play it.
			//  Function "runProcess()" will do all of this for us.
			fileStore.setPathname(pathname.toString());
			fileStore.runProcess();
			//  Give the process some time to get started, otherwise this sample will
			//  exit before it does (a regular program does not need to do this!)
			Thread.sleep(5000);
		}
		catch(Exception ignore) {}
		
		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
