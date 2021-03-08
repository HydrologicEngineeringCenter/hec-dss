#include "stdio.h"
#include "string.h"

#include "heclib.h"
#include "hecdss7.h"
#include "zStructTimeSeries.h"
#include "hecdssInternal.h"
#include "TestDssC.h"


	//  Irregular interval floats, basic

int testExpandedTimesIrreg(long long *ifltab)
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

	tss1 = zstructTsNewIrregFloats("/Basin/Location/Flow//IR-Century/Large Time Span/", fvalues, isize, itimes, DAY_GRANULARITY, "21Jan1000", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testExpandedTimesIrreg Loc 1, store status ")) return status; 
	//ztsMessTimeWindow(ifltab, 0, tss1);

	julian = dateToJulian("21Jan1000");
	julian += itimes[isize-1];
	julian += 100;   //  a little slack at the end
	julianToDate(julian, 4, cdate, sizeof(cdate));
	tss2 = zstructTsNewTimes("/Basin/Location/Flow//IR-Century/Large Time Span/", "01Jan1000", "0001", cdate, "2400"); 

	//  Force day granularity
	tss2->timeGranularitySeconds = DAY_GRANULARITY;
	ztsProcessTimes(ifltab, tss2, 0);
	//ztsMessTimeWindow(ifltab, 0, tss2);
	status = ztsRetrieve(ifltab, tss2,  0, 1, 0);
	ztsMessTimeWindow(ifltab, 0, tss2);
	for (i=0; i<10; i++) {
//		ztsIrregMessage(ifltab, 0, "tss2 return", tss2->times[i], tss2->timeGranularitySeconds, 
//			tss2->julianBaseDate, &tss2->floatValues[i], 1, 1);
	}

	if (zcheckStatus(ifltab, status, 1, "Fail in testExpandedTimesIrreg Loc 2, retrieve status ")) return status; 

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testExpandedTimesIrreg, Location 3");
	if (status) return status;


	zstructFree(tss2);
	//  Now try using a granual size of hours
	julian = dateToJulian("21Jan1000");
	julian += itimes[isize-1];
	julian += 100;   //  a little slack at the end
	julianToDate(julian, 4, cdate, sizeof(cdate));
	tss2 = zstructTsNewTimes("/Basin/Location/Flow//IR-Century/Large Time Span/", "01Jan1000", "0001", cdate, "2400"); 

	//  Force day granularity
	tss2->timeGranularitySeconds = HOUR_GRANULARITY;
	//ztsProcessTimes(ifltab, tss2, 0);
	//ztsMessTimeWindow(ifltab, 0, tss2);
	status = ztsRetrieve(ifltab, tss2,  0, 1, 0);
	ztsMessTimeWindow(ifltab, 0, tss2);
	for (i=0; i<10; i++) {
//		ztsIrregMessage(ifltab, 0, "tss2 return", tss2->times[i], tss2->timeGranularitySeconds, 
//			tss2->julianBaseDate, &tss2->floatValues[i], 1, 1);
	}

	if (zcheckStatus(ifltab, status, 1, "Fail in testExpandedTimesIrreg Loc 4, retrieve status ")) return status; 

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testExpandedTimesIrreg, Location 5");
	if (status) return status;

	zstructFree(tss1);
	zstructFree(tss2);
	free(itimes);
	free(fvalues);
	tss1 = 0;
	tss2 = 0;	


	return 0; 
}

