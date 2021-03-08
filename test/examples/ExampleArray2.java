import hec.heclib.dss.*;
import hec.io.ArrayContainer;

//  Example using HecDssArray to write and read various data types
//
public class ExampleArray2 {

	public static void main (String args[])  {
	
		HecDssArray dssArray = new HecDssArray();
		int status = dssArray.setDSSFileName("C:/temp/Example7.dss");
		if (status != 0) {
			System.out.println("Cannot access DSS file: C:/temp/Sample7.dss" );
			return;
		}
		
		//  Save our last computation information
		//  (Only one array per pathname)
		ArrayContainer lastPoint = new ArrayContainer();
		lastPoint.setName("/Red/Bend/Current Computation Point///Study 1/");
		int intArray[] = new int[1];
		intArray[0] = 1234;
		lastPoint.setIntArray(intArray);					
		status = dssArray.write(lastPoint);
		if (status != 0) {
			System.out.println("Error in int array store");
			return;
		}
		
		//  Now save float input, then double results
		ArrayContainer input = new ArrayContainer();
		input.setName("/Red/Bend/Input///Study 1/");		
		//  Gen up data
		float floatArray[] = new float[50];
		for (int i=0; i<floatArray.length; i++) {
			floatArray[i] = (float)i;
		}
		input.setFloatArray(floatArray);
		
		ArrayContainer results = new ArrayContainer();
		results.setName("/Red/Bend/Results///Study 1/");
		double doubleArray[] = new double[75];
		for (int i=0; i<doubleArray.length; i++) {
			doubleArray[i] = (double)(i * 100);
		}
		results.setDoubleArray(doubleArray);

		//  Store 
		status = dssArray.write(input);
		if (status != 0) {
			System.out.println("Error storing input");
			return;
		}
		status = dssArray.write(results);
		if (status != 0) {
			System.out.println("Error storing results");
			return;
		}
		dssArray.done();
		
		//  Read data
		HecDssArray dssReadArray = new HecDssArray();
		status = dssReadArray.setDSSFileName("C:/temp/Example7.dss");
		if (status != 0) {
			System.out.println("Cannot access DSS file: C:/temp/Sample7.dss" );
			return;
		}
		
		ArrayContainer lastPointRead = new ArrayContainer();
		lastPointRead.setName(lastPoint.getName());
		ArrayContainer inputRead = new ArrayContainer();
		inputRead.setName(input.getName());
		ArrayContainer resultsRead = new ArrayContainer();
		resultsRead.setName(results.getName());
		
		status = dssReadArray.read(lastPointRead);
		if (status != 0) {
			System.out.println("Error reading last point");
			return;
		}
		status = dssReadArray.read(inputRead);
		if (status != 0) {
			System.out.println("Error reading input");
			return;
		}
		status = dssReadArray.read(resultsRead);
		if (status != 0) {
			System.out.println("Error reading results");
			return;
		}
		dssReadArray.done();
		
		//  Now use the data.
		if (lastPointRead.isIntArray() == true) {
			int iarray[] = lastPointRead.getIntArray();
			System.out.println("Last computation point: " + iarray[0]);
		}
		if (inputRead.isFloatArray() == true) {
			float farray[] = inputRead.getFloatArray();
			for (int i=0; i<farray.length; i++) {
				System.out.println(farray[i]);
			}
		}
		if (resultsRead.isDoubleArray() == true) {
			double darray[] = resultsRead.getDoubleArray();
			for (int i=0; i<darray.length; i++) {
				System.out.println(darray[i]);
			}
		}		
		
		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
