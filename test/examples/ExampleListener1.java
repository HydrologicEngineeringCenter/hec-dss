
import hec.event.HecDssEventType;
import hec.heclib.dss.*;

//  An example listening for  "Flow" records changed in the file
//
public class ExampleListener1 implements hec.event.HecDssListener
{	
	HecDssCatalog catalog;

	public static void main (String args[])  {
		new ExampleListener1();
	}
	
	public ExampleListener1()
	{	
		catalog = new HecDssCatalog("C:/temp/Sample7.dss");
		catalog.addHecDssListener(this, "/*/*/Flow/*/1Hour/*/", HecDssEventType.RecordsChanged);
	}

	public void hecDssEventPerformed(HecDssEventType source) 
	{
		String[] paths = catalog.getPathnames();
		if ((paths != null) && (paths.length > 0)) {			
			for (int i=0; i<paths.length; i++) {
				System.out.println("Record changed, pathname: " + paths[i]);									
			}  
		}
	}

	public void done()
	{
		catalog.removeHecDssListener(this);
	}


}
