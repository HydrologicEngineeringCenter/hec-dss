#include <stdio.h>
#include <string.h>
#include "heclib.h"

#include "TestDssC.h"


int TestPairedData3(long long *ifltab)
{
	zStructPairedData *pds1, *pds2, *pds3, *pds4, *pds5;
	float fordinates[500], fvalues[10][500];
	float *temp;
	char label[20], labels[100];
	int labelLength;
	int len;
	char pathname[100];
	int numberValues;
	int status, i, j;


	//  This function tests features not supported in DSS-6
	  if (zgetVersion(ifltab) == 6) {
		  return 0;
	  }

	//  Gen up some data
	for (j=0; j<500; j++) {
		fordinates[j] = (float)(j + 1) * (float)10;
		for (i=0; i<10; i++) {
			fvalues[i][j] = ((float)(j + 1) * (float)100.0) + (float)(i + 1);
		}
	}
	
	stringCopy(pathname, sizeof(pathname), "/Basin/Location/Stage-Damage///PD 3/", _TRUNCATE);
	pds1 = zstructPdNewFloats(pathname, fordinates, (float *)fvalues, 500, 10,
								"feet", "linear", "dollars", "linear");
	status = zpdStore(ifltab, pds1, 0);		
	if (status != STATUS_OKAY) return status;	
//zset("mlvl", "", 15);
	
	//  Now retrieve the data
	pds2 = zstructPdNew(pathname);
	status = zpdRetrieve(ifltab, pds2, 0);
	if (status) return status;
	// compare
	status = comparePDs (ifltab, pds1, pds2, "TestPairedData3, Loc 1, floats");
	if (status != STATUS_OKAY) return status; 

	//  Lets change a block of data within the curves
	//  It's easiest (not effiecent) to retrieve the data,
	//  change and store back.  Let's do that for row 5-10, columns 2-5:
	pds3 = zstructPdNew(pathname);
	pds3->startingOrdinate = 4;
	pds3->endingOrdinate = 10;
	pds3->startingCurve = 2;
	pds3->endingCurve = 5;
	status = zpdRetrieve(ifltab, pds3, 1);

	//  We'll just use this as a single array for simplicity,
	//  instead of the doubly dimensioned array, which it really is.
	numberValues = pds3->numberCurvesInStruct * pds3->numberOrdinatesInStruct;
	//  Let's  make the values negative
	for (i=0; i<numberValues; i++) {
		pds3->floatValues[i] = -(pds3->floatValues[i]);
	}

	status = zpdStore(ifltab, pds3, 0);	
	if (status != STATUS_OKAY) return status;

	//  Now read and print the earlier block
	pds4 = zstructPdNew(pathname);
	pds4->startingOrdinate = pds3->startingOrdinate;
	pds4->endingOrdinate = pds3->endingOrdinate;
	pds4->startingCurve = pds3->startingCurve;
	pds4->endingCurve = pds3->endingCurve;
	status = zpdRetrieve(ifltab, pds4, 0);
	status = comparePDs (ifltab, pds3, pds4, "TestPairedData3, Loc 10, A segment");
	if (status != STATUS_OKAY) return status; 

	//  Compare the full data set now
	//  pds2 should be the original pds1, but not what is on disk
	//  Change pds2 to be what should be on disk, read, and compare
	for (j=pds4->startingOrdinate; j<=pds4->endingOrdinate ; j++) {
		for (i=pds4->startingCurve; i<=pds4->endingCurve ; i++) {
			fvalues[i-1][j-1] = -fvalues[i-1][j-1];
		}
	}
	temp = pds2->floatValues;
	pds2->floatValues = (float *)fvalues;
	pds5 = zstructPdNew(pathname);
	status = zpdRetrieve(ifltab, pds5, 0);
	// compare
	status = comparePDs (ifltab, pds2, pds5, "TestPairedData3, Loc 11, floats");
	if (status) return status;
	pds2->floatValues = temp;

	zstructFree(pds1);
	zstructFree(pds2);
	zstructFree(pds3);
	zstructFree(pds4);
	zstructFree(pds5);
	printf("Completed testPairedData3 test successfully!\n");
	return 0; 
}
