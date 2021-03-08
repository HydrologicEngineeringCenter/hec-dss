#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "TestDssC.h"


	//  Regular interval floats, basic

int testTimeSeriesPattern(long long *ifltab)
{
	zStructTimeSeries *tss1, *tss2;
	float fvalues[200];
	double dvalues[200];
	int times[200];
	int i;
	int status;
	int zero = 0;

	for (i=0; i<200; i++) {
		fvalues[i] = (float)i;
		dvalues[i] = (double)i;
		times[i] = 10000 + i * 10;
	}

	printf("enter testTimeSeriesPattern\n");
	tss1 = zstructTsNewRegFloats("/Basin/Location/Flow/TS-Pattern/1Hour/Float Pattern/", fvalues, 200, "21Jan2001", "1200", "cfs", "Inst-Val");
	tss1->startJulianDate = UNDEFINED_TIME;
	tss1->startTimeSeconds = -1;
	tss1->dataType = DATA_TYPE_RTS_PATTERN;
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testTimeSeriesPattern Loc 1, store status ")) return status; 

	tss2 = zstructTsNew("/Basin/Location/Flow/TS-Pattern/1Hour/Float Pattern/"); 
	status = ztsRetrieve(ifltab, tss2, 0, 0, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testTimeSeriesPattern Loc 2, retrieve status ")) return status; 

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testTimeSeriesPattern, Location 3");
	if (status) return status;

	zstructFree(tss1);
	zstructFree(tss2);

	//  Other variations not supported by DSS-6
	if (zgetVersion(ifltab) == 7) {
		tss1 = zstructTsNewRegDoubles("/Basin/Location/Flow/TS-Pattern/1Hour/Double Pattern/", dvalues, 200, "21Jan2001", "1200", "cfs", "Inst-Val");
		tss1->startJulianDate = UNDEFINED_TIME;
		tss1->startTimeSeconds = -1;
		status = ztsStore(ifltab, tss1, 0);
		if (zcheckStatus(ifltab, status, 1, "Fail in testTimeSeriesPattern Loc 11, store status ")) return status; 

		tss2 = zstructTsNew("/Basin/Location/Flow/TS-Pattern/1Hour/Double Pattern/"); 
		status = ztsRetrieve(ifltab, tss2, 0, 0, 0);
		if (zcheckStatus(ifltab, status, 1, "Fail in testTimeSeriesPattern Loc 12, retrieve status ")) return status; 

		status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testTimeSeriesPattern, Location 13");
		if (status) return status;

		zstructFree(tss1);
		zstructFree(tss2);

	/*				int retrieveDoublesFlag - 4th argument
	*					A flag indicating if floats or doubles should be returned.  This is independent of
	*					what is actually stored on disk!  Values will be converted to the requested type.
	*						0:  Return data as stored.  If missing, will return as doubles.
	*						1:  Return floats
	*						2:  Return doubles
	*/

		//  test double to float conversion
		tss1 = zstructTsNew("/Basin/Location/Flow/TS-Pattern/1Hour/Float Pattern/"); 
		status = ztsRetrieve(ifltab, tss1, 0, 1, 0);
		if (zcheckStatus(ifltab, status, 1, "Fail in testTimeSeriesPattern Loc 21, retrieve status ")) return status; 

		tss2 = zstructTsNew("/Basin/Location/Flow/TS-Pattern/1Hour/Double Pattern/"); 
		status = ztsRetrieve(ifltab, tss2, 0, 1, 0);
		if (zcheckStatus(ifltab, status, 1, "Fail in testTimeSeriesPattern Loc 22, retrieve status ")) return status; 

		//  We need to over-ride the obvious differences between the structs
		tss2->dataType = 101;

		status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testTimeSeriesPattern, Location 23");
		if (status) return status;

		zstructFree(tss1);
		zstructFree(tss2);


		//  test float to double conversion
		tss1 = zstructTsNew("/Basin/Location/Flow/TS-Pattern/1Hour/Float Pattern/"); 
		status = ztsRetrieve(ifltab, tss1, 0, 2, 0);
		if (zcheckStatus(ifltab, status, 1, "Fail in testTimeSeriesPattern Loc 31, retrieve status ")) return status; 

		tss2 = zstructTsNew("/Basin/Location/Flow/TS-Pattern/1Hour/Double Pattern/"); 
		status = ztsRetrieve(ifltab, tss2, 0, 2, 0);
		if (zcheckStatus(ifltab, status, 1, "Fail in testTimeSeriesPattern Loc 32, retrieve status ")) return status; 

		//  We need to over-ride the obvious differences between the structs
		tss1->dataType = 106;

		status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testTimeSeriesPattern, Location 33");
		if (status) return status;

		zstructFree(tss1);
		zstructFree(tss2);

	}


	/*
	tss1 = zstructTsNewRegFloats("/Basin/Location/Flow//1Hour/TSS-Floats/", fvalues, 200, "21Jan2001", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testTimeSeriesPattern Loc 1, store status ")) return status; 


	tss2 = zstructTsNew("/Basin/Location/Flow/01Jan2001/1Hour/TSS-Floats/"); 
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testTimeSeriesPattern Loc 2, retrieve status ")) return status; 

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testTimeSeriesPattern, Location 3");
	if (status) return status;

	zstructFree(tss1);
	zstructFree(tss2);
	/*

	tss1 = zstructTsNewRegFloats("/Basin/Location/Flow//1Hour/User Header/", fvalues, 200, "21Jan2001", "1200", "cfs", "Inst-Val");

	tss1->userHeader = (int *)calloc(5,4);
	tss1->userHeader[0] = 1;
	tss1->userHeader[1] = 2;
	tss1->userHeader[2] = 3;
	tss1->userHeader[3] = 4;
	tss1->userHeader[4] = 5;
	tss1->userHeaderNumber = 5;

	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testTimeSeriesPattern Loc 10, store status ")) return status; 


	tss2 = zstructTsNew("/Basin/Location/Flow/01Jan2001/1Hour/User Header/"); 
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testTimeSeriesPattern Loc 11, retrieve status ")) return status; 

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testTimeSeriesPattern, Location 12");
	if (status) return status;

	free(tss1->userHeader);
	tss1->userHeader = 0;
	zstructFree(tss1);
	zstructFree(tss2);

	*/

	return 0; 
}

