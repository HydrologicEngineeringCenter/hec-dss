#include <stdio.h>
#include <string.h>

#include "heclib.h"
#include "hecdssInternal.h"
#include "zStructTimeSeries.h"
#include "TestDssC.h"

// If MacOS, use hec_zopen instead of stdio::zopen
#ifdef __APPLE__
#define zopen hec_zopen
#endif

int miscTests()
{
	long long ifltab[250];
	char ctime[20];
	zStructTimeSeries *tss1;

	int status;
	
	status = STATUS_OKAY;

	status = zopen(ifltab, "Sample7.dss");
	if (status != STATUS_OKAY) return status;

	tss1 = zstructTsNewTimes("/Basin/Location/Flow//1Hour/Java Sample/", "20Jan2010", "1000", "30Jan2010", "1200"); 
	status = ztsRetrieve(ifltab, tss1, -1, 1, 0);
	if (status != STATUS_OKAY) return status; 

	secondsToTimeString(tss1->startTimeSeconds, 0, 0,  ctime, sizeof(ctime));
	printf("Time = %s\n", ctime);

	return status;
}

