
import hec.heclib.util.HecTime;
import hec.io.DataContainer;

//  A short example of getting a catalog or list of pathnames in a DSS file
//
public class  ExampleLocationStuff {

	public static void add(Object container) {
		if (container instanceof DataContainer) {
		}
		else {
			System.out.println("No data container");
			DataContainer dc = (DataContainer)container;
			
			dc.coordinateSystem = 1;
			dc.coordinateID = 2;
			dc.xOrdinate = 11;
			dc.yOrdinate = 12;
			dc.zOrdinate = 13;
			dc.horizontalDatum = 1;
			dc.horizontalUnits = 2;
			dc.verticalDatum = 2;
			dc.verticalUnits = 1;
			dc.supplementalInfo = "This is some supplemental information";
		}
	}

	public static void print(Object container) {
		if (container instanceof DataContainer) {
			DataContainer dc = (DataContainer)container;
			
			System.out.println("Location info...\n");
			System.out.println("fullName: " + dc.fullName);	
			System.out.println("fileName: " + dc.fileName);	
			
			System.out.println("Data Type: " + dc.dataType);
			System.out.println("Record last write time: " + dc.lastWriteTimeMillis);
			HecTime ht = new HecTime(HecTime.SECOND_GRANULARITY);
			ht.setTimeInMillis(dc.lastWriteTimeMillis);
			System.out.println("Record last write time: " + ht.dateAndTime());
			System.out.println("File last write time: " + dc.lastWriteTimeMillis);
			ht.setTimeInMillis(dc.lastWriteTimeMillis);
			System.out.println("File last write time: " + ht.dateAndTime());
			
			//  Input
			System.out.println("Coordinate System: " + dc.coordinateSystem);
			System.out.println("coordinateID: " + dc.coordinateID);
			System.out.println("xOrdinate:  " + dc.xOrdinate);
			System.out.println("yOrdinate:  " + dc.yOrdinate);
			System.out.println("zOrdinate:  " + dc.zOrdinate);
			System.out.println("horizontalUnits: " + dc.horizontalUnits);
			System.out.println("horizontalDatum: " + dc.horizontalDatum);
			System.out.println("verticalUnits: " + dc.verticalUnits);
			System.out.println("verticalDatum: " + dc.verticalDatum);
			System.out.println("supplementalInfo: " + dc.supplementalInfo);			
		
		}
		else {
			System.out.println("No data container");
		}
	}
}
