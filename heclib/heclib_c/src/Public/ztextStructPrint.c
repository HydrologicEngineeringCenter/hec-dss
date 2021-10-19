#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "zdssKeys.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"


/**
*  Function:	ztextStructPrint
*
*  Use:			Public
*
*  Description:	Print the contents of a text struct to standard out.  Utility function
*
*  Declaration: void ztextStructPrint(zStructText *textStruct);
*
*  Parameters:	zStructText *textStruct
*					The text struct to print to standard out
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/

void ztextStructPrint(zStructText *textStruct)
{
	int i, len;
	int count;
	int nrows, ncols;
	int r, c;


	nrows = textStruct->numberRows;
	ncols = textStruct->numberColumns;

	//  Print the textString
	if ((textStruct->numberTextChars > 0) && textStruct->textString) {
		printf("\n%s\n", textStruct->textString);
	}

	//  Print labels (column headers)
	if ((textStruct->numberLabelChars > 0) && textStruct->labels) {
		count = 0;
		printf(" |");
		for (i = 0; i<ncols; i++) {
			printf("%s|", &textStruct->labels[count]);
			len = strnlen_hec(&textStruct->labels[count], textStruct->numberLabelChars - count);
			count += len + 1;
			if (count >= textStruct->numberLabelChars) break;
		}
		printf("\n");
	}

	if ((textStruct->numberTableChars > 0) && textStruct->textTable) {

		//  Print the text table by row, column
		count = 0;
		for (r = 0; r < nrows; r++) {
			printf(" |");
			for (c = 0; c < ncols; c++) {
				printf("%s|", &textStruct->textTable[count]);
				len = strnlen_hec(&textStruct->textTable[count], textStruct->numberTableChars - count);
				count += len + 1;
			}
			printf("\n");
		}
	}
}

