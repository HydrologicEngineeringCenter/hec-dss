#include "heclib.h"

int SampleTimeSeries()
{
	long long ifltab[250];
	zStructTimeSeries *tss1, *tss2, *tss3, *tss4;
	float fvalues[200];
	double dvalues[300];
	int itimes[300];
	int status, i;

	//  Open the DSS file; Create if it doesn't exist
	status = zopen(ifltab, "SampleTimeSeries.dss");
	if (status != STATUS_OKAY) return status;

	//  Write a regular interval data set.  Gen up the data
	for (i=0; i<200; i++) {
		fvalues[i] = (float)i;
	}
	tss1 = zstructTsNewRegFloats("/Basin/Location/Flow//1Hour/Test/", fvalues, 200, "21Jan2001", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	zstructFree(tss1);
	if (status != STATUS_OKAY) return status; 

	//  Write a irregular interval data set.  Gen up the data
	for (i=0; i<300; i++) {
		dvalues[i] = (double)i;
		itimes[i] = i * 1440;
	}
	tss2 = zstructTsNewIrregDoubles("/Basin/Location/Flow//~1Day/Sample/", dvalues, 300, itimes, MINUTE_GRANULARITY, "20April2012", "cfs", "Inst-Val");	
	status = ztsStore(ifltab, tss1, 0);
	zstructFree(tss2);
	if (status != STATUS_OKAY) return status; 

	//  Read the data back in
	tss3 = zstructTsNew("/Basin/Location/Flow/01Jan2001/1Hour/Test/"); 
	status = ztsRetrieve(ifltab, tss3, -1, 0, 0);
	if (status != STATUS_OKAY) return status;  

	tss4 = zstructTsNew("/Basin/Location/Flow//~1Day/Sample/"); 
	status = ztsRetrieve(ifltab, tss4, -1, 0, 0);
	if (status != STATUS_OKAY) return status;

	zstructFree(tss3);
	zstructFree(tss4);
	zclose(ifltab);
	return 0; 
}

