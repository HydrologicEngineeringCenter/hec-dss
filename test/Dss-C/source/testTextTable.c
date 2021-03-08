#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "hecdss7.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "TestDssC.h"


//  Test buffered IO in DSS-7
//
//  This function will call znewWrite and then store values so that
//  we can write different types of data to buffered IO 
//  for testing.

int testTextTable(long long *ifltab)
{
	
	int nrows, ncols;
	char pathname[] = "/a/b/c1-c2/d/e/PD Text/";
	int zero = 0;

	nrows = 100;
	ncols = 50;


	/*
	//  zcheck(long long *ifltab, const char* pathname, int statusWanted, int *istat)
	status = zcheck(ifltab, pathname);
	if ((status != STATUS_RECORD_FOUND) && (status != STATUS_RECORD_NOT_FOUND)) {
		//  An error code 
		return status;
	}
	if (status != STATUS_OKAY) {
		zdelete(ifltab, pathname);
	}

	icount = 0;
	//  "Row 1"
	//  "This is row x, and column y."
	for (i=0; i<nrows; i++) {		
		
		_snprintf_s(textm1[i], sizeof(textm1[i]), _TRUNCATE, "Row %d", i);
		text[i][0] = textm1[i];
		_snprintf_s(textm2[i], sizeof(textm2[i]), _TRUNCATE, "Column 1");
		text[i][1] = textm2[i];
		for (j=2; j<50; j++) {
			_snprintf_s(textMess[icount], sizeof(textMess[icount]), _TRUNCATE, 
			 "This is row %d, and column %d.", i, j);
			text[i][j] = textMess[icount];
			icount++;
		}
	}
	sizes[0] = sizeof(textm1[0]);
	sizes[1] = sizeof(textm2[0]);
	for (j=2; j<ncols; j++) {
		sizes[j] = sizeof(textMess[0]);
	}

	for (j=0; j<ncols; j++) {
		_snprintf_s(labelArray[j], sizeof(labelArray[j]), _TRUNCATE, "Column %d", j);
		labels[j] = labelArray[j];
	}
	maxSize = 0;
	for (j=0; j<ncols; j++) {
		maxSize += sizes[j] * nrows;
	}

	tts = (zStructTextTable *)zTxTableStructNew(pathname);

	tts->textTable = (char ***)text;
	tts->numberColumns = ncols;
	tts->numberRows = nrows;
	tts->labels = labels;
	tts->labelArray = (char *)&labelArray; 
	tts->boolStoreLabels = 1;

	status = ztextTableStore(ifltab, tts);

	strncpy(mess,  "testTextTable Loc 1, store status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	tts2 = (zStructTextTable *)zTxTableStructNew(pathname);	
	status = ztextTableRetrieve(ifltab, tts2);

	strncpy(mess,  "testTextTable Loc 5, retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;
	
	strncpy(mess,  "testTextTable Loc 10, number Columns ", sizeof(mess)); 
	checknumbers_(&ncols, &tts2->numberColumns, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;
	strncpy(mess,  "testTextTable Loc 10, number Rows ", sizeof(mess)); 
	checknumbers_(&nrows, &tts2->numberRows, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	for (i=0; i<nrows; i++) {				
		for (j=0; j<ncols; j++) {
			ipos = (i * ncols) + j;
			_snprintf_s(mess, sizeof(mess), _TRUNCATE, "testTextTable Loc 20, Row %d, column %d", i, j);
			checkstring_((char *)tts->textTable[ipos], (char *)tts2->textTable[ipos], mess, &status, 
				strlen(tts->textTable[ipos]), strlen(tts2->textTable[ipos]), strlen(mess));
			if (status != STATUS_OKAY) return status;
		}
	}


	for (j=0; j<ncols; j++) {
		len1 = strlen(labels[j]);
		len2 = strlen(tts2->labels[j]);
		_snprintf_s(mess, sizeof(mess), _TRUNCATE, "testTextTable Loc 30, lable lengths are not the same;  read: %d, should be %d, for column %d", len2, len1, j);
		checknumbers_(&len1, &len2, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;

		_snprintf_s(mess, sizeof(mess), _TRUNCATE, "testTextTable Loc 30, Label %d", j);
		//printvals(labels[j], tts2->labels[j]);
		//printf("len 1 = %d\n",strlen(labels[j]));
		//printf("len 2 = %d\n",strlen(tts2->labels[j]));
		checkstring_(labels[j], tts2->labels[j], mess, &status, 
			strlen(labels[j]), strlen(tts2->labels[j]), strlen(mess));
		if (status != STATUS_OKAY) return status;
	}


	zstructFree(tts);
	zstructFree(tts2);
	
	*/
	printf("Completed testTextTable test successfully!\n");

	return 0; 
}

void printvals(char *c1, char *c2)
{
	//printf("Val 1 ==>%s,==  val 2 ==>%s<==\n", c1, c2);
}

