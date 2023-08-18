#include <stdlib.h>

#include "heclib.h"
#include "TestDssC.h"



//  Regular interval floats, basic but second granularity 

int testztsStruct14(long long *ifltab)
{
	zStructTimeSeries *tss1, *tss2;
	float fvalues[2000];
	double dvalues[300];
	int itimes[300];
	int i;
	int status;
	int zero = 0;

	for (i = 0; i < 2000; i++) {
		fvalues[i] = (float)i;
	}

	//  This function tests features not supported in DSS-6
	if (zgetVersion(ifltab) == 6) {
		return 0;
	}
	//zset("MLVL", "", 12);
	tss1 = zstructTsNewRegFloats("/Basin/Location/Flow//10Seconds/TSS-Floats/", fvalues, 2000, "21Jan2001", "2300", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct14 Loc 1, store status ")) return status;


	tss2 = zstructTsNew("/Basin/Location/Flow/21Jan2001/10Seconds/TSS-Floats/");
	tss2->startJulianDate = tss1->startJulianDate;
	tss2->startTimeSeconds = 1;
	tss2->endJulianDate = tss1->endJulianDate;
	tss2->endTimeSeconds = SECS_IN_1_DAY;
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct14 Loc 2, retrieve status ")) return status;

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testztsStruct14, Location 3");
	if (status) return status;

	zstructFree(tss1);
	zstructFree(tss2);

	tss1 = zstructTsNewRegFloats("/Basin/Location/Flow//10Seconds/User Header/", fvalues, 200, "21Jan2001", "2300", "cfs", "Inst-Val");

	tss1->userHeader = (int *)calloc(6, 4);
	tss1->userHeader[0] = 1;
	tss1->userHeader[1] = 2;
	tss1->userHeader[2] = 3;
	tss1->userHeader[3] = 4;
	tss1->userHeader[4] = 5;
	tss1->userHeader[5] = 6;
	tss1->userHeaderNumber = 6;

	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct14 Loc 10, store status ")) return status;


	tss2 = zstructTsNew("/Basin/Location/Flow/21Jan2001/10Seconds/User Header/");
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct14 Loc 11, retrieve status ")) return status;

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testztsStruct14, Location 12");
	if (status) return status;

	free(tss1->userHeader);
	zstructFree(tss1);
	zstructFree(tss2);

	//  Now test irregular interval data with seconds
	for (i = 0; i < 300; i++) {
		dvalues[i] = (double)i;
		if (i % 2 == 0) {
			itimes[i] = i * SECS_IN_1_MINUTE + 10;
		}
		else {
			itimes[i] = i * SECS_IN_1_MINUTE - 10;
		}
	}

	tss1 = zstructTsNewIrregDoubles("/Basin/Location/Flow//Ir-Day/Second Granularity/",
		dvalues, 300, itimes, SECOND_GRANULARITY, "01May2010", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct14 Loc 20, store status ")) return status;

	//  Read the data back in
	tss2 = zstructTsNew("/Basin/Location/Flow/01May2010/Ir-Day/Second Granularity/");
	status = ztsRetrieve(ifltab, tss2, 0, 2, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct14 Loc 21, retrieve status ")) return status;

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testztsStruct14, Location 22");
	if (status) return status;

	zstructFree(tss1);
	zstructFree(tss2);

	return 0;
}

