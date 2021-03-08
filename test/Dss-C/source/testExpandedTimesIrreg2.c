#include "stdio.h"
#include "string.h"

#include "heclib.h"
#include "hecdss7.h"
#include "zStructTimeSeries.h"
#include "hecdssInternal.h"
#include "TestDssC.h"


	//  Irregular interval floats, basic

int testExpandedTimesIrreg2(long long *ifltab)
{
	zStructTimeSeries *tss1, *tss2;
	//float fvalues[200000];
	float *fvalues;
	//int itimes[200000];
	int *itimes;
	int i;
	int j;
	int status;
	int julian;
	int mins;
	int zero = 0;
	char *cnull=0;
	char cdate[20];
	int isize;


	//  Not compatible with version 6
	if (zgetVersion(ifltab) != 7) return 0;
	//return 0;

	isize = 2000000;
	itimes = (int *)malloc(isize * 4);
	if (!itimes) {
		printf("failed malloc of large allocation, itimes:  %d\n", isize);
		return -1;
	}
	fvalues = (float *)malloc(isize * 4);
	if (!fvalues) {
		printf("failed malloc of large allocation, fvalues:  %d\n", isize);
		return -1;
	}

	//  A value every 10 days from year 1,000, about 5,500 years
	for (i=0; i<isize; i++) {
		fvalues[i] = (float)i;
		itimes[i] = i * 10;
	}
	//zsetMessageLevel(11, MESS_LEVEL_INTERNAL_DIAG_1);
	//tss1 = zstructTsNewIrregFloats("/Basin/Location/Flow//IR-Century/Extended Time Span/", fvalues, isize, itimes, DAY_GRANULARITY, "20Jan-5000", "cfs", "Inst-Val");
	tss1 = zstructTsNewIrregFloats("/Basin/Location/Flow//IR-Century/Extended Time Span/", fvalues, isize, itimes, DAY_GRANULARITY, "20Jan2000", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testExpandedTimesIrreg2 Loc 1, store status ")) return status; 
	
	tss2 = zstructTsNew("/Basin/Location/Flow//IR-Century/Extended Time Span/"); 
	tss2->boolRetrieveAllTimes = 1;
	status = ztsRetrieve(ifltab, tss2,  0, 1, 0);
	ztsMessTimeWindow(ifltab, 0, tss2);

	if (zcheckStatus(ifltab, status, 1, "Fail in testExpandedTimesIrreg2 Loc 2, retrieve status ")) return status; 

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testExpandedTimesIrreg2, Location 3");
	if (status) return status;

	zstructFree(tss1);
	zstructFree(tss2);
	free(itimes);
	free(fvalues);
	tss1 = 0;
	tss2 = 0;	


	return 0; 
}

