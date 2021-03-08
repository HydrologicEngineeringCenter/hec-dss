
#include "heclib.h"
#include "TestDssC.h"



	//  Irregular interval floats, basic but second granularity 

int testztsStruct15(long long *ifltab)
{
	zStructTimeSeries *tss1, *tss2;
	float fvalues[2000];
	int itimes[2000];
	int i;
	int status;
	int zero = 0;

	for (i=0; i<2000; i++) {
		fvalues[i] = (float)i;
		itimes[i] = (23 * 3600) + (i * 10); 
	}

	//  This function tests features not supported in DSS-6
	  if (zgetVersion(ifltab) == 6) {
		  return 0;
	  }
	//zset("MLVL", "", 12);
	tss1 = zstructTsNewIrregFloats("/Basin/Location/Flow//IR-Day/TSS-Floats/", fvalues, 2000, itimes, MINUTE_GRANULARITY, "21Jan2001", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct15 Loc 1, store status ")) return status; 


	tss2 = zstructTsNew("/Basin/Location/Flow//IR-Day/TSS-Floats/"); 
	tss2->startJulianDate = tss1->startJulianDate;
	tss2->startTimeSeconds = 1;
	tss2->endJulianDate = tss1->endJulianDate;
	tss2->endTimeSeconds = 24 * 60 * 60;
	tss2->timeGranularitySeconds = MINUTE_GRANULARITY;
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct15 Loc 2, retrieve status ")) return status; 

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testztsStruct15, Location 3");
	if (status) return status;

	zstructFree(tss1);
	zstructFree(tss2);


	return 0; 
}

