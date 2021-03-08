import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Vector;

import hec.gfx2d.G2dData;
import hec.gfx2d.PlotDialogFactory;
import hec.gfx2d.TimeSeriesDataSet;
import hec.heclib.dss.*;
import hec.io.TimeSeriesContainer;

//  An example where you are listening to a data set to
//  determine if you need to refresh a plot
//
public class ExampleListener2 implements hec.event.HecDssListener
{	
	TimeSeriesContainer timeSeriesContainer;
	TimeSeriesDataSet timeSeriesDataSet;
	HecTimeSeries dssTimeSeries;

	public static void main (String args[])  {
		new ExampleListener2();
	}
	
	public ExampleListener2()
	{	
		dssTimeSeries = new HecTimeSeries("C:/temp/Sample7.dss");
		timeSeriesContainer = new TimeSeriesContainer();
		//  You can specify the entire dataset (over time) by using a "*" (time will be searched for)
		//  timeSeriesContainer.fullName = "//SACRAMENTO/TEMP-MAX/*/1DAY/OBS/";
		//  But using the actual dates is much faster (no searching needed)
		timeSeriesContainer.setName("//SACRAMENTO/TEMP-MAX/11Jul1877 - 30Jun2009/1DAY/OBS/");		
		int status = dssTimeSeries.read(timeSeriesContainer, true);
		if (status != 0) return;

		//  Show in a plot		
		timeSeriesDataSet = new TimeSeriesDataSet(timeSeriesContainer);
		Vector<G2dData> dataVec = new Vector<G2dData>();	
		dataVec.add(timeSeriesDataSet);
		hec.gfx2d.G2dDialog plot = PlotDialogFactory.getPlotDialogCreator().createPlotDialog(null, timeSeriesContainer.fullName, false, dataVec);
		plot.setVisible(true);
		
		dssTimeSeries.addHecDssListener(this);
		final ExampleListener2 listener = this;
		plot.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				dssTimeSeries.removeHecDssListener(listener);
				dssTimeSeries.close();
			}
		});
				
		//  This happens somewhere else in your program
	/* * /	try { Thread.sleep(10000);}
		catch (Exception ignore){}
		HecTimeSeries ts = new HecTimeSeries("C:/temp/Sample7.dss");		
		TimeSeriesContainer tsc  = new TimeSeriesContainer();
		tsc.setName("//SACRAMENTO/TEMP-MAX/01Jan1960/1DAY/OBS/";
		status = ts.read(tsc, true);
		tsc.values[50] = 135.0;
		//  this will fire the event
		status = ts.write(tsc);
	/ * */		
	}

	public void hecDssEventPerformed(hec.event.HecDssEventType source) 
	{
		//  If you want to know the change type:
		//  int changeType = source.getChangeType();
		//  Since we specified the dataset (pathnames), only those
		//  will be listened for (and notified when they are changed)
		int status = dssTimeSeries.read(timeSeriesContainer, true);
		if (status == 0) {
			//  When you change TimeSeriesDataSet, that will tell the plot to refresh
			timeSeriesDataSet.setData(timeSeriesContainer);
		}		
	}
}
