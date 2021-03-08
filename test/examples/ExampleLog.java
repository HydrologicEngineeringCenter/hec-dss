import javax.swing.UIManager;

import hec.heclib.dss.*;
import hec.util.TextDialog;

public class ExampleLog {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}
		
		//  This will create a log file in the users temp directory
		int status = HecDataManager.setLogFile("MyProgramName");
		if (status != 0) return;
		HecDataManager.setMessageLevel(4);  //  Optional - this shows reads as well as writes		
				
		//  Do some DSS stuff here
		HecDataManager dataManager = new HecDataManager();
		dataManager.setDSSFileName("C:/temp/Sample7.dss");
				
		//  Display the dss log
		TextDialog textDialog = HecDataManager.displayLogFile(null);
		if (textDialog != null) {
			textDialog.setVisible(true);
		}
		HecDataManager.closeLogFile();
	}
}
