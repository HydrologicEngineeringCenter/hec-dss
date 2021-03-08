import javax.swing.UIManager;

import hec.heclib.dss.*;
import hec.io.TimeSeriesContainer;


public class ExampleSimpleErrorProcessing {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}
	
		HecTimeSeries dataManager = new HecTimeSeries();
		String filename = "C:/temp/Sample7.dss";
		try {						
			dataManager.open(filename, false, 7);
			if (dataManager.checkForError() != 0) {
				DSSErrorMessage errorMessage = dataManager.getLastError();    		
				dataManager.clearError();
				if ((errorMessage != null) || (errorMessage.severity > 0)) {
					System.out.println(errorMessage.message());
					if (errorMessage.severity > 3) return;
				}
			}
		}
		catch (Exception except) {
			System.out.println(except);
			return;
		}

		try {
			//  Create an error by trying to write an empty TimeSeriesContainer
			int status = dataManager.write(new TimeSeriesContainer());
			if (status < 0) {
				DSSErrorMessage errorMessage = dataManager.getLastError();    		
				dataManager.clearError();			
				System.out.println(errorMessage.message());
				if (errorMessage.severity > 3) return;
			}
		}
		catch (Exception except) {
			System.out.println(except);
		} 

		HecDataManager.closeAllFiles();	
	}
}