#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "hecdss7.h"
#include "zStructTimeSeries.h"
#include "hecdssInternal.h"
#include "TestDssC.h"



//  Regular interval doubles, basic

int testztsStruct2(long long *ifltab)
{
	zStructTimeSeries *tss1, *tss2;
	double dvalues[200];
	int i;
	int status;
	int zero = 0;

	for (i=0; i<200; i++) {
		dvalues[i] = (double)i;
	}

	//zsetMessageGroupLevel("tsread", 6);
	//zsetMessageGroupLevel("tswrite", 6);
	//zsetMessageGroupLevel("general", 10);

	tss1 = zstructTsNewRegDoubles("/Basin/Location/Flow//1Hour/TSS-Doubles/", dvalues, 200, "21Jan2001", "1200", "cfs", "Inst-Val");
	////////////////
	//  Location Data
	//tss1->locationStruct =  zstructLocationNew(0);
	//tss1->allocated[zSTRUCT_TS_locationStruct] = 1;
	stringCopy (tss1->locationStruct->timeZoneName, sizeof(tss1->locationStruct->timeZoneName), "PST", 3);
	tss1->locationStruct->xOrdinate = -100;
    tss1->locationStruct->yOrdinate = 101;
    tss1->locationStruct->zOrdinate = 102;
    tss1->locationStruct->coordinateSystem = 2;
	tss1->locationStruct->coordinateID = 3;
    tss1->locationStruct->horizontalUnits = 4;
    tss1->locationStruct->horizontalDatum=5;
    tss1->locationStruct->verticalUnits=1;
    tss1->locationStruct->verticalDatum = 2; 
	status = ztsStore(ifltab, tss1, 0);	
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct2 Loc 1, store status ")) return status; 

	zprintFileInfo(ifltab);

	tss2 = zstructTsNew("/Basin/Location/Flow/01Jan2001/1Hour/TSS-Doubles/"); 
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct2 Loc 2, retrieve status ")) return status; 

	//  Since we are asking for floats and doubles are in tss1, the compare would fail...
	//  So we need to convert the doubles to floats before comparing
	tss2->doubleValues = (double *)calloc(tss2->numberValues, 8);
	convertDataArray((void *)tss2->floatValues, (void *)tss2->doubleValues, tss2->numberValues, 1, 2);
	free(tss2->floatValues);
	tss2->floatValues = 0;
	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testztsStruct2, Location 3");
	if (status) return status;
	free(tss2->doubleValues);

	zstructFree(tss1);
	zstructFree(tss2);

	return 0; 
}

