#include <stdio.h>
#include "heclib.h"

// If MacOS, use hec_zopen instead of stdio::zopen
#ifdef __APPLE__
#define zopen hec_zopen
#endif

int main()
{
	long long ifltab[250];
	zStructTimeSeries *tss2;
	double dvalues[2];
	int itimes[2];
	int status;

	dvalues[0] = 1.1;
	dvalues[1] = 1.2;

	itimes[0] = 0;
	itimes[1] = 1440;

     zsetMessageLevel(10,20);
	status = zopen(ifltab, "ExampleSecondGranularity.dss");
	if (status != STATUS_OKAY) return status;

	tss2 = zstructTsNewIrregDoubles("/Basin/Location/Flow//~1Day/C Example/", dvalues, 2, itimes, SECOND_GRANULARITY, "20April2012", "cfs", "Inst-Val");	
	status = ztsStore(ifltab, tss2, 0);
	zstructFree(tss2);
	if (status != STATUS_OKAY) return status; 	

 	zclose(ifltab);
	return 0; 
}

