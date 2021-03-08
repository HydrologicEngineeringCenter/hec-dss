import java.io.File;
import java.util.Vector;

import hec.heclib.dss.*;
import hec.util.PhotoViewerInfo;
import hec.util.PhotoViewerJFrame;


//  Save and display a timed slide show of pictures using HEC Photo Viewer
//
public class ExampleImage2 {

	public static void main (String args[])  {
	
		try {						
			HecDataManager.setDefaultDSSFileName("C:/temp/VacationPictures.dss");
			//  Be sure we can create / access the DSS file.  Do it here, so we don't
			//  have to bother with in the loop (below)
			HecDataManager dataManager = new HecDataManager();
			int status = dataManager.open();
			if (status != 0) {
				System.out.println("Cannot access DSS file: " + HecDataManager.defaultDSSFileName());
				return;
			}
			//  Store all the sample pictures included with Windows
			File dir = new File("C:/Users/Public/Pictures/Sample Pictures");
			Vector<File> picts = hec.util.FileUtilities.listAllFiles(dir); 
			for (int i=0; i<picts.size(); i++) {
				String fileName = picts.elementAt(i).getAbsolutePath();
				if (fileName.endsWith(".jpg")) {
					//  HecDssImage will figure out the pathname from the file name for us.
					//  (Cannot reuse HecDssImage here)
					HecDssImage dssImage = new HecDssImage();
					dssImage.setAPart("My Summer Vacation");
					status = dssImage.write(picts.elementAt(i));
					if (status != 0) {
						System.out.println("Error storing " + dssImage.pathname());
						return;
					}
					dssImage.done();
				}
			}
			//  Get a list of the image pathnames
			HecDssCatalog catalog = new HecDssCatalog();
			String paths[] = catalog.getCatalog(false, "/*/*/*/IMAGE/*/*/");
			//  Now give a timed slide show using HEC PhotoViewer 
			PhotoViewerJFrame imageFrame = null; 
			for (int i=0; i<paths.length; i++) {
				PhotoViewerInfo pvInfo = new PhotoViewerInfo();
				HecDssImage dssImage = new HecDssImage();
				dssImage.setPathname(paths[i]);
				status = dssImage.read(pvInfo);
				if (status != 0) {
					System.out.println("Error retrieving " + dssImage.pathname());
					return;
				}
				dssImage.done();
				//  Since we are using a timer, not a "Next" button, we tell the frame
				//  we only have one image (at a time)
				if (imageFrame == null) {
					imageFrame = new PhotoViewerJFrame(pvInfo);
					imageFrame.addMenu();
					imageFrame.setVisible(true);
				}
				else {
					imageFrame.addNextImage(pvInfo, false);
					imageFrame.nextImage();
					imageFrame.getPhotoViewer().readImage();
					imageFrame.getPhotoViewer().paint();
				}
				try {
					Thread.sleep(10000);
				}
				catch (Exception e) {}
			}
			if (imageFrame != null) {
				imageFrame.setVisible(false);
				imageFrame.dispose();
			}
			imageFrame = null;
		}
		catch(Exception e) {
			System.out.println("Exception: " + e);
		}
		
		//  If you add in the mp3 playing from the File examples, you'll
		//  have your own show for family and friends!
		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
