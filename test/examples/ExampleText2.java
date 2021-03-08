import javax.swing.UIManager;

import hec.dataTable.TextTable;
import hec.heclib.dss.*;
import hec.io.TextContainer;

//  Write a text table
public class ExampleText2 {

	public static void main (String args[])  {
		String alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRTSUVWXYZ";
		try {
			javax.swing.UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ignore) {}
		
		HecDssText dssText1 = new HecDssText();
		dssText1.setDSSFileName("C:/temp/Example7.dss");		
		TextContainer textContainer1 = new TextContainer();	
		textContainer1.setName("/Group/Location/Text Table/d/e/f/");		
		String textTable[][] = new String[20][5];
		for (int i=0; i<20; i++) {
			int n = i/25;
			n = i - (n*25) + 1;			
			for (int j=0; j<5; j++) {
				textTable[i][j] = alpha.substring((j+2), (n+8));
			}
		}
		textContainer1.setTextTable(textTable);
		
		//  Gen column headers
		String labels[] = new String[5];
		for (int j=0; j<5; j++) {
			labels[j] = "Col " + Integer.toString(j+1);
		}
		textContainer1.setLabels(labels);
		
		textContainer1.setText(alpha + "\n" + alpha + "\n" + alpha + "\n" + alpha);	
		
		int status = dssText1.write(textContainer1);
		dssText1.done();
		if (status < 0) return;	
		
		HecDssText dssText2 = new HecDssText();
		dssText2.setDSSFileName("C:/temp/Example7.dss");		
		TextContainer textContainer2 = new TextContainer();	
		textContainer2.setName("/Group/Location/Text Table/d/e/f/");
		status = dssText2.read(textContainer2);
		dssText2.done();
		if (status < 0) return;
		if (textContainer2.hasText()) {
			System.out.println("text ==>" + textContainer2.getText() + "<==");
		}

		if (textContainer2.hasTextTable()) {
			TextTable frame = new TextTable(null, textContainer2);							
			frame.setSize(800, 600); 
			frame.setVisible(true);	
		}
	}
}
