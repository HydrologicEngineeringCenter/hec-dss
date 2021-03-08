import javax.swing.UIManager;

import hec.dataTable.TextTable;
import hec.heclib.dss.*;
import hec.io.TextContainer;

/*
 * Store and retrive a simple text string.  Display string in text table
 */
public class ExampleText1 {

	public static void main (String args[])  {

		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}

		HecDssText dssText1 = new HecDssText();
		dssText1.setDSSFileName("C:/temp/Example7.dss");	
		TextContainer textContainer1 = new TextContainer();	
		textContainer1.setName("/Group/Location/Message/d/e/f/");
		textContainer1.setText("This is a text message that is written to HEC-DSS");		
		int status = dssText1.write(textContainer1);
		dssText1.done();
		if (status < 0) return;
		
		HecDssText dssText2 = new HecDssText();
		dssText2.setDSSFileName("C:/temp/Example7.dss");		
		TextContainer textContainer2 = new TextContainer();	
		textContainer2.setName("/Group/Location/Message/d/e/f/");
		status = dssText2.read(textContainer2);
		dssText2.done();
		if (status < 0) return;
		HecDataManager.closeAllFiles();
		
		if (textContainer2.text != null) {
			System.out.println("text ==>" + textContainer2.getText() + "<==");
		}
		
		//  The visual TextTable works with either a single line of text or a text table
		TextTable frame = new TextTable(null, textContainer2);							
		frame.setSize(500, 400); 
		frame.setVisible(true);			
	}
}
