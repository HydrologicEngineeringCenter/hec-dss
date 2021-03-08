import java.io.File;
import hec.heclib.dss.*;
import hec.util.PhotoViewerInfo;
import hec.util.PhotoViewerJFrame;


//  An example of storing an .jpg file in DSS, then retrieving and playing with Media Player
//
public class ExampleImage1 {

	public static void main (String args[])  {
	
		try {
			HecDssImage dssImage = new HecDssImage();
			int status = dssImage.setDSSFileName("C:/temp/Sample7.dss");
			if (status != 0) {
				System.out.println("Cannot access DSS file: C:/temp/Sample7.dss" );
				return;
			}

			//  Use the sample photos included with Windows
			File file = new File("C:/Users/Public/Pictures/Sample Pictures/Desert.jpg");
			DSSPathname pathname = new DSSPathname();
			//  Set the C part to the file name, D part to "IMAGE" and E part to the extension 
			pathname.setCPart("Desert.jpg");
			pathname.setDPart("IMAGE");
			pathname.setEPart("jpg");
			System.out.println("Pathname: " + pathname.toString());
			//  Write to DSS
			status = dssImage.write(file, pathname);
			if (status != 0) {
				System.out.println("Error storing " + pathname);
				return;
			}

			//  Use function "runProcess()" to display with default program (Windows Photo Viewer)
			dssImage.setPathname(pathname.toString());
			dssImage.runProcess();
			//  Give the process some time to get started
			Thread.sleep(5000);
			
			//  Now display using HEC PhotoViewer (which we have control over)
			PhotoViewerInfo pvInfo = new PhotoViewerInfo();
			status = dssImage.read(pvInfo);
			if (status != 0) {
				System.out.println("Error retrieving " + pathname);
				return;
			}
			PhotoViewerJFrame image = new PhotoViewerJFrame(pvInfo);
			image.setVisible(true);
		}
		catch(Exception e) {
			System.out.println("Exception: " + e);
		}
		
		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
