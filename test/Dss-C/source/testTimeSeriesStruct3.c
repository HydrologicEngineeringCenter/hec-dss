#include "stdio.h"
#include "string.h"

#include "heclib.h"
#include "hecdss7.h"
#include "zStructTimeSeries.h"
#include "hecdssInternal.h"
#include "TestDssC.h"


	//  Irregular interval floats, basic

int testztsStruct3(long long *ifltab)
{
	zStructTimeSeries *tss1, *tss2;
	float fvalues[200];
	int itimes[200];
	int i;
	int status;
	int julian;
	int mins;
	int zero = 0;
	char *cnull=0;

	julian = dateToJulian("21Jan2001");
	mins = julian * MINS_IN_1_DAY + MINS_IN_12_HOURS;
	for (i=0; i<200; i++) {
		fvalues[i] = (float)i;
		itimes[i] = mins +(i * 60);
	}

	tss1 = zstructTsNewIrregFloats("/Basin/Location/Flow//~1Hour/TSS-Floats/", fvalues, 200, itimes, MINUTE_GRANULARITY, cnull, "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct3 Loc 1, store status ")) return status; 

	tss2 = zstructTsNew("/Basin/Location/Flow/01Jan2001/~1Hour/TSS-Floats/"); 
	status = ztsRetrieve(ifltab, tss2,  0, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct3 Loc 2, retrieve status ")) return status; 

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testztsStruct3, Location 3");
	if (status) return status;

	zstructFree(tss1);
	zstructFree(tss2);

	return 0; 
}

