#include <stdio.h>
#include "heclib.h"

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
	status = zopen(ifltab, "ExampleMinuteGranularity.dss");
	if (status != STATUS_OKAY) return status;

	tss2 = zstructTsNewIrregDoubles("/Basin/Location/Flow//~1Day/C Example/", dvalues, 2, itimes, MINUTE_GRANULARITY, "20April2012", "cfs", "Inst-Val");	
	status = ztsStore(ifltab, tss2, 0);
	zstructFree(tss2);
	if (status != STATUS_OKAY) return status; 	

 	zclose(ifltab);
	return 0; 
}

