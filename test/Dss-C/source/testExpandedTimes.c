#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "TestDssC.h"


	//  Regular interval floats, basic

int testExpandedTimes(long long *ifltab)
{
	zStructTimeSeries *tss1, *tss2;
	float fvalues[20000];
	int i;
	int status;
	int zero = 0;

	for (i=0; i<20000; i++) {
		fvalues[i] = (float)i;
	}

	//  Not compatible with version 6
	if (zgetVersion(ifltab) != 7) return 0;
	//return 0;

	tss1 = zstructTsNewRegFloats("/Basin/Location/Flow//1Hour/ExpandedTimes/", fvalues, 20000, "01Jan50000", "1200", "cfs", "Inst-Val");
	tss1->julianBaseDate = tss1->startJulianDate;
	tss1->startJulianDate = 0;
	tss1->startTimeSeconds = 43200;
	tss1->endJulianDate = 0;
	tss1->endTimeSeconds = 0;
	//zset("MLVL", "", 10);
	ztsMessTimeWindow((long long*)ifltab, DSS_FUNCTION_javaNativeInterface_ID, tss1);
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testExpandedTimes Loc 1, store status ")) return status; 


	tss2 = zstructTsNew("/Basin/Location/Flow/01Jan50001/1Hour/ExpandedTimes/"); 
	tss2->boolRetrieveAllTimes = 1;
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testExpandedTimes Loc 2, retrieve status ")) return status; 

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testExpandedTimes, Location 3");
	if (status) return status;

	zstructFree(tss1);
	zstructFree(tss2);
	
	



	return 0; 
}

