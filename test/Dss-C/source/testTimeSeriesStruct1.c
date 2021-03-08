#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "TestDssC.h"


	//  Regular interval floats, basic

int testztsStruct1(long long *ifltab)
{
	zStructTimeSeries *tss1, *tss2;
	float fvalues[200];
	int i;
	int status;
	int zero = 0;

	for (i=0; i<200; i++) {
		fvalues[i] = (float)i;
		//fvalues[i] = 1234;
	}

/*
	tss1 = zstructTsNewRegFloats("/Basin/Location/Flow//30Min/TSS-Floats/", fvalues, 200, "21Jan5000001", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct1 Loc 1, store status ")) return status; 


	tss2 = zstructTsNew("/Basin/Location/Flow/01Jan5000001/30Min/TSS-Floats/"); 
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct1 Loc 2, retrieve status ")) return status; 

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testztsStruct1, Location 3");
	if (status) return status;

	zstructFree(tss1);
	zstructFree(tss2);
	*/

	tss1 = zstructTsNewRegFloats("/Basin/Location/Flow//30Min/TSS-Floats/", fvalues, 200, "21Jan2001", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct1 Loc 1, store status ")) return status; 


	tss2 = zstructTsNew("/Basin/Location/Flow/01Jan2001/30Min/TSS-Floats/"); 
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct1 Loc 2, retrieve status ")) return status; 

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testztsStruct1, Location 3");
	if (status) return status;

	zstructFree(tss1);
	zstructFree(tss2);

	if (getEndian() && (zgetVersion(ifltab) == 6)) {
		printf("Unable to test user header for version 6 on big endian machines.\n");
		printf("Version 6 user header is character; version 7 is ints\n");
		return 0;
	}
	/**/

	tss1 = zstructTsNewRegFloats("/Basin/Location/Flow//30Min/User Header/", fvalues, 200, "21Jan2001", "1200", "cfs", "Inst-Val");

	tss1->userHeader = (int *)calloc(6,4);
	tss1->userHeader[0] = 1;
	tss1->userHeader[1] = 2;
	tss1->userHeader[2] = 3;
	tss1->userHeader[3] = 4;
	tss1->userHeader[4] = 5;
	tss1->userHeader[5] = 6;
	tss1->userHeaderNumber = 6;

	if (getEndian() && (zgetVersion(ifltab) == 7)) {
		zswitchInts(tss1->userHeader, 6);
	}

	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct1 Loc 10, store status ")) return status; 

	if (getEndian() && (zgetVersion(ifltab) == 7)) {
		zswitchInts(tss1->userHeader, 6);
	}

	tss2 = zstructTsNew("/Basin/Location/Flow/01Jan2001/30Min/User Header/"); 
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct1 Loc 11, retrieve status ")) return status; 

	if (getEndian() && (zgetVersion(ifltab) == 7)) {
		zswitchInts(tss2->userHeader, 6);
	}

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testztsStruct1, Location 12");
	if (status) return status;

	free(tss1->userHeader);
	tss1->userHeader = 0;
	zstructFree(tss1);
	zstructFree(tss2);



	return 0; 
}

