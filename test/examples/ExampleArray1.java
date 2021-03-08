import hec.heclib.dss.*;
import hec.io.ArrayContainer;

//  An example of saving a generic array in a DSS file
/*
 *
 * public class ArrayContainer extends DataContainer implements Cloneable, Serializable
{
	public static final int INT_ARRAY = 90;
	public static final int FLOAT_ARRAY = 91;
	public static final int DOUBLE_ARRAY = 92;
		
	public int intArray[];
	public float floatArray[];
	public double doubleArray[];	
 */
//
public class ExampleArray1 {

	public static void main (String args[])  {
	
		HecDssArray dssArray = new HecDssArray();
		int status = dssArray.setDSSFileName("C:/temp/Example.dss");
		if (status != 0) {
			System.out.println("Cannot access DSS file: C:/temp/Example.dss" );
			return;
		}
		
		//  ONLY one array per container		
		double doubleArray[] = new double [100];
		for (int i=0; i<100; i++) {
			doubleArray[i] = 100.0 + ((double)i * 10.0);
		}
		ArrayContainer results = new ArrayContainer();
		results.setName("/Red/Outlet/Results/June 2040//Study 1/");
		results.setDoubleArray(doubleArray);
		status = dssArray.write(results);
		if (status != 0) {
			System.out.println("Error storing array");
			return;
		}
				
		//  Now read our dataset and print
		//  Usually we would create a new HecDssArray object, but for 
		//  a simple write or read, it's not necessary
		ArrayContainer studyResults = new ArrayContainer();
		studyResults.setName("/Red/Outlet/Results/June 2040//Study 1/");
		status = dssArray.read(studyResults);
		if (status != 0) {
			System.out.println("Error retrieving array");
			return;
		}

		if (studyResults.isIntArray() == true) {
			int intArray[] = studyResults.getIntArray();
			System.out.println("int array found, length = " + intArray.length);
			for (int i=0; i<intArray.length; i++) {
				System.out.println(intArray[i]);
			}
		}
		else if (studyResults.isFloatArray() == true) {
			float floatArray[] = studyResults.getFloatArray();
			System.out.println("float array found, length = " + floatArray.length);
			for (int i=0; i<floatArray.length; i++) {
				System.out.println(floatArray[i]);
			}
		}
		else if (studyResults.isDoubleArray() == true) {
			double dArray[] = studyResults.getDoubleArray();
			System.out.println("double array found, length = " + dArray.length);
			for (int i=0; i<dArray.length; i++) {
				System.out.println(dArray[i]);
			}
		}
		else {
			System.out.println("No arrays found.");
		}
		dssArray.done();

		HecDataManager.closeAllFiles();  //  Only at the end of the program
	}
}
