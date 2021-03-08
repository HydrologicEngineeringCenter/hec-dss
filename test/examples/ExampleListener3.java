
import hec.event.HecDssEventType;
import hec.heclib.dss.*;

//  An example listening for new "Flow" records added to the file
//
public class ExampleListener3 implements hec.event.HecDssListener
{	
	HecDataManager dataManager;

	public static void main (String args[])  {
		new ExampleListener3();
	}
	
	public ExampleListener3()
	{	
		dataManager = new HecDataManager("C:/temp/Sample7.dss");
		//  We are only interested in hourly flow records
		dataManager.addHecDssListener(this, "/*/*/Flow/*/1Hour/*/", HecDssEventType.RecordsAdded);
	}

	public void hecDssEventPerformed(HecDssEventType source) 
	{
		String[] paths = dataManager.getPathnames();
		if ((paths != null) && (paths.length > 0)) {
			for (int i=0; i<paths.length; i++) {
				System.out.println("New record added, pathname: " + paths[i]);					
			}  
		}
	}

	public void done()
	{
		dataManager.removeHecDssListener(this);
	}

}
