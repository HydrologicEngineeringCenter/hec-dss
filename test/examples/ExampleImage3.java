import java.awt.event.ActionEvent;
import java.io.File;
import java.util.Observable;
import java.util.Observer;
import java.util.Vector;

import hec.heclib.dss.*;
import hec.util.PhotoViewerInfo;
import hec.util.PhotoViewerJFrame;


//  Save and give a slide show using the Next button
//
public class ExampleImage3 implements Observer {
	
	String _pn[] = null;
	PhotoViewerJFrame _iFrame = null; 
	int _count;
	
	public static void main (String args[])  {
		new ExampleImage3();
	}
	
	public ExampleImage3() 
	{	
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
			//  A slide show where the user presses Next each time
			_count = 0;
			_pn = catalog.getCatalog(false, "/*/*/*/IMAGE/*/*/");
			nextSlideShow();
		}
		catch(Exception e) {
			System.out.println("Exception: " + e);
		}	
		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
	
	public void nextSlideShow()
	{
		if (_count == _pn.length) {
			if (_iFrame != null) {
				_iFrame.setVisible(false);
				_iFrame.dispose();
				_iFrame = null;
			}
			return;
		}
		PhotoViewerInfo pvInfo = new PhotoViewerInfo();
		HecDssImage dssImage = new HecDssImage();
		dssImage.setPathname(_pn[_count++]);
		int status = dssImage.read(pvInfo);
		if (status != 0) {
			System.out.println("Error retrieving " + dssImage.pathname());
			return;
		}
		dssImage.done();
		if (_iFrame == null) {
			_iFrame = new PhotoViewerJFrame(pvInfo, _pn.length);
			_iFrame.addMenu();
			_iFrame.addObserver(this);
			_iFrame.setVisible(true);
		}
		else {
			boolean moreImages = false;
			if (_count < (_pn.length)) moreImages = true;
			_iFrame.addNextImage(pvInfo, moreImages);
		}		
	}
	
	public void update(Observable o, Object arg)
	{
		if (arg instanceof ActionEvent) {
			ActionEvent event = (ActionEvent) arg;
			// event.
			String eventName = event.getActionCommand();
			if (eventName.compareTo("nextImage") == 0) {
				nextSlideShow();
			}
		}
	}
	
}
