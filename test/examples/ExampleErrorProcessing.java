import java.io.File;
import java.io.RandomAccessFile;
import javax.swing.JOptionPane;
import javax.swing.UIManager;

import hec.heclib.dss.*;
import hec.io.TimeSeriesContainer;


public class ExampleErrorProcessing {

	protected static boolean _showDialog = true;

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}

		//  Try to create a file with an invalid name or location
		//  This error is caught in the Java code and throws an exception	
		HecTimeSeries dataManager = new HecTimeSeries();
		String filename = "Z:/temp/Sample7.dss";
		try {						
			dataManager.open(filename, false, 7);
			if (dataManager.checkForError() != 0) {
				if (processError(dataManager)) return;
			}
		}
		catch (Exception except) {
			//  We will end up here
			System.out.println(except);
		}


		//  Create a DSS file, then clobber the file header to create an error
		filename = "C:/temp/ExampleErrorProcess.dss";
		dataManager = new HecTimeSeries();
		try {	
			File f = new File(filename);
			f.delete();
			dataManager.open(filename, false, 7);
			if (dataManager.checkForError() != 0) {				
				if (processError(dataManager)) return;
			}
			dataManager.close();
			RandomAccessFile raf = new RandomAccessFile(filename, "rw");
			raf.seek(10);
			raf.writeChars("Im a string that is clobbering this DSS file.");
			raf.close();			
			dataManager.open(filename, false, 7);
			if (dataManager.checkForError() != 0) {
				//  We will end up here
				if (processError(dataManager)) return;
			}
		}
		catch (Exception except) {			
			System.out.println(except);
		}


		//  Delete it and make it a valid file, then 
		//  Try to write a time series data set with nothing in it
		try {
			File f = new File(filename);
			f.delete();
			dataManager.setDSSFileName(filename, false, 7);
			if (dataManager.checkForError() != 0) {
				if (processError(dataManager)) return;
			}
			int status = dataManager.write(new TimeSeriesContainer());
			if (dataManager.checkForError() != 0) {
				if (processError(dataManager)) return;
			}
		}
		catch (Exception except) {
			System.out.println(except);
		} 

		HecDataManager.closeAllFiles();	
		System.out.println("Completed Example");
	}



	public static boolean processError(HecDataManager dataManager)
	{
		if (dataManager == null) {
			String message = "Unknown Error / Unable to access file";
			System.out.println(message.toString());
			if (_showDialog) {
				JOptionPane.showMessageDialog(null, message,
						" Critical Error",
						JOptionPane.ERROR_MESSAGE);    			
			}
			return false;
		}
		DSSErrorMessage errorMessage = dataManager.getLastError();    		
		dataManager.clearError();
		if ((errorMessage == null) || (errorMessage.errorType == 0)) {
			return false;
		}
		//  An error has occurred, get message
		StringBuffer message = new StringBuffer();
		if (errorMessage.errorType < 3) {
			buildErrorMessage(message, errorMessage, _showDialog);
			return true;
		}
		else {
			buildErrorMessage(message, errorMessage, false);
			message.append("\nProgram must exit.\nPress OK to exit or Cancel to attempt to continue.");
			System.out.println(message.toString());
			if (_showDialog) {
				int option = JOptionPane.showConfirmDialog(null, message.toString(),
						"Critical Error",
						JOptionPane.OK_CANCEL_OPTION,
						JOptionPane.ERROR_MESSAGE);
				if (option == JOptionPane.CANCEL_OPTION) {
					return true;
				}
			}
			//  Don't save anything or close anything - just get out
			System.exit(-1);
			return true;   
		}
	}

	public static void buildErrorMessage(StringBuffer message, DSSErrorMessage errorMessage, boolean showDialog)
	{
		int icon = JOptionPane.INFORMATION_MESSAGE;
		if (errorMessage.errorType == 1) {
			message.append("Warning:\n");
			icon = JOptionPane.WARNING_MESSAGE;
		}
		else if (errorMessage.errorType == 2) {
			message.append("File access error:\n");
			icon = JOptionPane.WARNING_MESSAGE;
		}
		else if (errorMessage.errorType == 3) {
			message.append("File corruption error:\n");
			icon = JOptionPane.ERROR_MESSAGE;
		}
		else if (errorMessage.errorType == 4) {
			message.append("Critical error:\n");
			icon = JOptionPane.ERROR_MESSAGE;
		}
		if (errorMessage.errorMessage.trim().length() > 2) {
			message.append(errorMessage.errorMessage);
		}
		if (errorMessage.systemErrorMessage.trim().length() > 2) {
			message.append("\nSystem message: ");
			message.append(errorMessage.systemErrorMessage.trim());
		}
		if (errorMessage.systemError > 0) {
			message.append("\nSystem error code: ");
			message.append(errorMessage.systemError);
		}
		if (errorMessage.lastPathname.trim().length() > 2) {
			message.append("\nPathname: ");
			message.append(errorMessage.lastPathname);
		}
		if (errorMessage.dssFileName.trim().length() > 2) {
			message.append("\nFile: ");
			message.append(errorMessage.dssFileName);
		}
		message.append("\nSeverity: ");
		message.append(errorMessage.severity);
		if (errorMessage.errorType >= 1) {
			if (errorMessage.functionName.trim().length() > 2) {
				message.append("\nError occurred in function: ");
				message.append(errorMessage.functionName);
				if (errorMessage.calledByFunction.trim().length() > 2) {
					message.append("\nCalled by function: ");
					message.append(errorMessage.calledByFunction);
				}
			}
			if (errorMessage.lastAddress > 0) {
				message.append("\nat address: ");
				message.append(errorMessage.lastAddress);
			}
		}
		// message.append("\nError Code: ");
		// message.append(errorMessage.errorCode);
		System.out.println(message.toString());
		if (showDialog) {
			JOptionPane.showMessageDialog(null, message.toString(),
					" Error", icon);			
		}
	}
}
