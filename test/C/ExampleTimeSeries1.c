#include <stdio.h>
#include <string.h>
#include "heclib.h"

int main()
{
	long long ifltab[250];
	zStructTimeSeries *tss1, *tss2, *tss3, *tss4;
	float fvalues[200];
	double dvalues[300];
	int itimes[300];
	char cdate[13], ctime[10];
	int valueTime, status, i;

    memset(ifltab,0,sizeof(ifltab));
	//  Open the DSS file; Create if it doesn't exist
	status = hec_dss_zopen(ifltab, "ExampleTimeSeries1.dss");
	if (status != STATUS_OKAY) return status;

	//  Write a regular interval data set.  Gen up the data
	for (i=0; i<200; i++) {
		fvalues[i] = (float)i;
	}
	tss1 = zstructTsNewRegFloats("/Basin/Location/Flow//1Hour/C Test/", fvalues, 200, "21Jan2001", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	zstructFree(tss1);
	if (status != STATUS_OKAY) return status; 

	//  Write a irregular interval data set.  Gen up the data
	for (i=0; i<300; i++) {
		dvalues[i] = (double)i;
		itimes[i] = i * 1440;
	}
	tss2 = zstructTsNewIrregDoubles("/Basin/Location/Flow//~1Day/C Example/", dvalues, 300, itimes, MINUTE_GRANULARITY, "20April2012", "cfs", "Inst-Val");	
	status = ztsStore(ifltab, tss2, 0);
	zstructFree(tss2);
	if (status != STATUS_OKAY) return status; 	

	//  Read the data back in
	tss3 = zstructTsNew("/Basin/Location/Flow/01Jan2001/1Hour/C Test/"); 
	status = ztsRetrieve(ifltab, tss3, -1, 1, 0);
	if (status != STATUS_OKAY) return status; 
	//  Print out.  (Compute time for each ordinate, values are floats)
	valueTime = tss3->startTimeSeconds;
	for (i=0; i<tss3->numberValues; i++) {		
		getDateAndTime(valueTime, SECOND_GRANULARITY, tss3->startJulianDate, 
						cdate, sizeof(cdate), ctime, sizeof(ctime));
		printf ("Oridnate %d, for %s, %s, value is %f\n",i, cdate, ctime, tss3->floatValues[i]);
		valueTime += SECS_IN_1_HOUR;  //  Increment to next hour
	}

	tss4 = zstructTsNewTimes("/Basin/Location/Flow//~1Day/C Example/", "19April2012", "2400", "01July2013", "2400"); 
	status = ztsRetrieve(ifltab, tss4, 0, 2, 0);
	if (status != STATUS_OKAY) return status;
	//  Print out (values returned as doubles)
	for (i=0; i<tss4->numberValues; i++) {		
		getDateAndTime(tss4->times[i], tss4->timeGranularitySeconds, tss4->julianBaseDate, 
						cdate, sizeof(cdate), ctime, sizeof(ctime));
		printf ("Oridnate %d, for %s, %s, value is %f\n",i, cdate, ctime, tss4->doubleValues[i]);
	}

	zstructFree(tss3);
	zstructFree(tss4);
	zclose(ifltab);
	return 0; 
}

