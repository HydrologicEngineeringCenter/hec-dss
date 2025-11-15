#include <stdio.h>
#include <string.h>
#include "heclib.h"
#include "TestDssC.h"


int TestPairedData2(long long *ifltab)
{
	zStructPairedData *pds1, *pds2, *pds3, *pds4, *pds5, *pds6;
	zStructRecordSize *recordSize;
	double dordinates[500], dvalues[500];
	char pathname[100];

	int status, i, j;

	
	//  Pre-allocate space for 500 rows for a family of 10 curves
	//  Gen up the oridnates
	for (i=0; i<500; i++) {
		dordinates[i] = (double)(i + 1) * 10;
	}
	stringCopy(pathname, sizeof(pathname), "/Basin/Location/Stage-Damage///Test/", _TRUNCATE);
	pds1 = zstructPdNewDoubles(pathname, dordinates, (double *)dvalues, 500, 10,
								"feet", "linear", "dollars", "linear");
	//pds1->labels = 1;  //  b
	status = zpdStore(ifltab, pds1, 10);	// Flag 10 allocates space
	zstructFree(pds1);
	if (status != STATUS_OKAY) return status;

	//  Now write each of the 10 curves separately 
	for (j=0; j<10; j++) {
		for (i=0; i<500; i++) {
			dvalues[i] = ((double)(i + 1) * 100.0) + (double)(j + 1);
		}
		pds2 = zstructPdNew(pathname);
		//  First curve is #1, not #0 (by convention)
		pds2->startingCurve = j+1;
		pds2->endingCurve = pds2->startingCurve;
		pds2->doubleValues = dvalues;
		status = zpdStore(ifltab, pds2, 0);	
		zstructFree(pds2);
		if (status != STATUS_OKAY) return status;
	}

	//  Get the data type, number of ordinates and curves for this data set
	recordSize = zstructRecordSizeNew(pathname);
	status = zgetRecordSize (ifltab, recordSize);
	if (status != STATUS_OKAY) {
		zstructFree(recordSize);
		return status; 
	}
	zstructFree(recordSize);

	//  Now retrieve the data
	pds3 = zstructPdNew(pathname);
	status = zpdRetrieve(ifltab, pds3, 1);
	//  print a part of the struct	
	zstructFree(pds3);
	if (status != STATUS_OKAY) return status; 

	//  If we only wanted to retrieve row 5-7, columns 3-8, then:
	pds4 = zstructPdNew(pathname);
	pds4->startingOrdinate = 5;
	pds4->endingOrdinate = 7;
	pds4->startingCurve = 3;
	pds4->endingCurve = 8;
	status = zpdRetrieve(ifltab, pds4, 1);	
	zstructFree(pds4);
	if (status != STATUS_OKAY) return status;
	
	//  Retrieve a single curve
	pds5 = zstructPdNew(pathname);
	pds5->startingCurve = 3;
	pds5->endingCurve = 3;
	status = zpdRetrieve(ifltab, pds5, 1);	
	zstructFree(pds5);
	if (status != STATUS_OKAY) return status;

	//  Retrieve a single row (ordinate set)
	pds6 = zstructPdNew(pathname);
	pds6->startingOrdinate = 3;
	pds6->endingOrdinate = 3;
	status = zpdRetrieve(ifltab, pds6, 1);
	zstructFree(pds6);
	if (status != STATUS_OKAY) return status;

	printf("Completed testPairedData2 test successfully!\n");

	return 0; 
}
