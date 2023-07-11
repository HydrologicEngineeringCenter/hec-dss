#include "stdio.h"
#include "string.h"
#include "math.h"
#include <stdio.h>
//#include <process.h>


#include "heclib.h"
#include "hecdss7.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "TestDssC.h"



int testProfileIrreg(long long *ifltab)
{
	

	//  100 rows by 50 columns (curves)
	

	//  INPUT ONLY  - do not mix
	//   tssIn1 = floats,    tssIn2 = doubles
	zStructTimeSeries *tssIn1, *tssIn2;
	zStructTimeSeries *tssIn3;
	//  OUT ONLY
	zStructTimeSeries *tssOut1, *tssOut2;
	zStructTimeSeries *tss3;

	float depths[20];
	float values[1000][20];
	double ddepths[20];
	double dvalues[1000][20];
	double dvalues3[3000][20];
	int times[1000];
	int times3[3000];
	int itime;

	char mess[100];
	int i, j;
	int status;
	int nrows, ncols;
	char pathname1[] = "/a/b/Temperature-Depth/20Jun2020/~1Day/Irregular Float Profile Test/";
	char pathname2[] = "/a/b/Temperature-Depth/01Jan1968/~1Day/Irregular Double Profile Test/";
	int zero = 0;

	char startDate[20];
	char endDate[20];
	char startTime[20];
	char endTime[20];

	nrows = 100;
	ncols = 50;

	//zset("mlvl", "", 12);

	
	//tssOut1 = zstructTsNewTimes(pathname2, "", "", "", "");
	//status = ztsRetrieve(ifltab, tssOut1, 0, 0, 1);


	//  This function tests features not supported in DSS-6
	if (zgetVersion(ifltab) == 6) {
		return 0;
	}
	//  zcheck(long long *ifltab, const char* pathname, int statusWanted, int *istat)
	status = zcheck(ifltab, pathname1);
	if ((status != STATUS_RECORD_FOUND) && (status != STATUS_RECORD_NOT_FOUND)) {
		//  An error code 
		return status;
	}
	if (status != STATUS_OKAY) {
		zdelete(ifltab, pathname1);
	}
	status = zcheck(ifltab, pathname2);
	if ((status != STATUS_RECORD_FOUND) && (status != STATUS_RECORD_NOT_FOUND)) {
		//  An error code 
		return status;
	}
	if (status != STATUS_OKAY) {
		zdelete(ifltab, pathname2);
	}

	//  20 depth measurements, 3 meters apart with first at surface, last at 57 meters deep
	for (i=0; i<20; i++) {
		depths[i] = (float)(i);
		ddepths[i] = (double)depths[i];
	}

	//  We have 16 months of daily 8:00 am readings
	for (i=0; i<1000; i++) {
		times[i] = (25000 * MINS_IN_1_DAY) + (i * MINS_IN_1_DAY) + MINS_IN_8_HOUR;
		for (j=0; j<20; j++) {
			//values[i][j] = (20. - (float)j) + 35.0 + sin((float)i);
			values[i][j] = (float)(i * 1000) + j;
			dvalues[i][j] = (double)values[i][j];
		}
	}

	tssIn1 = zstructTsNew(pathname1);

	tssIn1->profileDepthsNumber = 20;
	tssIn1->numberValues = 1000;	
	tssIn1->floatProfileDepths = (float *)depths;
	tssIn1->floatProfileValues = (float *)values;
	tssIn1->times = times;
	tssIn1->unitsProfileDepths = "feet";
	tssIn1->unitsProfileValues = "deg F";
	tssIn1->type = "INST-VAL";

	tssIn2 = zstructTsNew(pathname2);

	tssIn2->profileDepthsNumber = 20;
	tssIn2->numberValues = 1000;	
	tssIn2->times = times;
	tssIn2->doubleProfileDepths = (double *)ddepths;
	tssIn2->doubleProfileValues = (double *)dvalues;
	tssIn2->unitsProfileDepths = "feet";
	tssIn2->unitsProfileValues = "deg F";
	tssIn2->type = "INST-VAL";

	// -------------------------------------------------------------------

	status = ztsStore(ifltab, tssIn1, 0);
	stringCopy(mess, sizeof(mess), "testProfileIrreg Loc 2, regular float store status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tssIn1->pathname);
		return status;
	}


	
	minsToDateTime(times[0], startDate, startTime, sizeof(startDate), sizeof(startTime));
	minsToDateTime(times[tssIn1->numberValues-1], endDate, endTime, sizeof(endDate), sizeof(endTime));

	//  Write floats, read floats
	tssOut1 = zstructTsNewTimes(pathname1, startDate, startTime, endDate, endTime);
	status = ztsRetrieve(ifltab, tssOut1, 0, 1, 1);
	stringCopy(mess, sizeof(mess), "testProfileIrreg Loc 4, regular retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = compareProfiles(ifltab, tssIn1, tssOut1, "Profile Float to Float, Loc 5");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, "\ntestProfileIrreg failed!\nLoc 6\n");
		zmessage(ifltab, tssIn1->pathname);
		return status;
	}

	zstructFree(tssOut1);

	// -------------------------------------------------------------------
	

	//  Now test getting doubles from doubles
	
	status = ztsStore(ifltab, tssIn2, 0);
	stringCopy(mess, sizeof(mess), "testProfileIrreg Loc 11, regular double store status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tssIn2->pathname);
		return status;
	}


	
	minsToDateTime(times[0], startDate, startTime, sizeof(startDate), sizeof(startTime));
	minsToDateTime(times[tssIn1->numberValues-1], endDate, endTime, sizeof(endDate), sizeof(endTime));

	//  Write doubles, read doubles
	tssOut2 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);
	status = ztsRetrieve(ifltab, tssOut2, 0, 2, 1);
	stringCopy(mess, sizeof(mess), "testProfileIrreg Loc 13, regular double retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tssOut2->pathname);
		return status;
	}

	status = compareProfiles(ifltab, tssIn2, tssOut2, "Profile double to double, Loc 14");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tssIn2->pathname);
		zmessage(ifltab, "\ntestProfileIrreg failed!\nLoc 15\n");
		return status;
	}

	zstructFree(tssOut2);

		//  Floats from doubles	
	minsToDateTime(times[0], startDate, startTime, sizeof(startDate), sizeof(startTime));
	minsToDateTime(times[tssIn1->numberValues-1], endDate, endTime, sizeof(endDate), sizeof(endTime));

	tssOut1 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);
	status = ztsRetrieve(ifltab, tssOut1, 0, 1, 1);
	stringCopy(mess, sizeof(mess), "testProfileIrreg Loc 51, regular double to float retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tssOut1->pathname);
		return status;
	}

	status = compareProfiles(ifltab, tssIn1, tssOut1, "Profile float to double, Loc 52");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, "\ntestProfileIrreg failed!\nLoc 53\n");
		zmessage(ifltab, tssIn1->pathname);
//		return status;
	}


	//  -----------------------------------------------------------------

	//  write a little bit of new data into the existing record

	for (i=0; i<10; i++) {
		times3[i] = times[0] + ((i-5)* MINS_IN_1_DAY);
		for (j=0; j<20; j++) {
			//values[i][j] = (20. - (float)j) + 35.0 + sin((float)i);
			//values[i][j] = (float)(i * 1000) + j;
			dvalues3[i][j] = (double)((i * 10) + j);
		}
	}
	tssIn3 = zstructTsNew(pathname2);
	

	tssIn3->profileDepthsNumber = 20;
	tssIn3->numberValues = 10;
	tssIn3->times = times3;
	tssIn3->doubleProfileDepths = (double *)ddepths;
	tssIn3->doubleProfileValues = (double *)dvalues3;
	tssIn3->unitsProfileDepths = "feet";
	tssIn3->unitsProfileValues = "deg F";
	tssIn3->type = "INST-VAL";

	minsToDateTime(times3[0], startDate, startTime, sizeof(startDate), sizeof(startTime));
	minsToDateTime(times3[tssIn3->numberValues - 1], endDate, endTime, sizeof(endDate), sizeof(endTime));

	status = ztsStore(ifltab, tssIn3, 0);
	stringCopy(mess, sizeof(mess), "testProfileIrreg Loc 21, regular double store status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;


	// 
	tssOut2 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);
	status = ztsRetrieve(ifltab, tssOut2, 0, 2, 1);
	stringCopy(mess, sizeof(mess), "testProfileIrreg Loc 23, regular double retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tssOut2->pathname);
		return status;
	}

	status = compareProfiles(ifltab, tssIn3, tssOut2, "Profile double to double, Loc 24");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tssIn3->pathname);
		zmessage(ifltab, "\ntestProfileIrreg failed!\nLoc 25\n");
		return status;
	}

	zstructFree(tssOut2);

	//   Now full data set

	minsToDateTime(times[tssIn1->numberValues-1], endDate, endTime, sizeof(endDate), sizeof(endTime));
	tssOut2 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);
	//zset("mlvl", "", 15);
	status = ztsRetrieve(ifltab, tssOut2, 0, 2, 1);
	stringCopy(mess, sizeof(mess), "testProfileIrreg Loc 27, regular double retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tssOut2->pathname);
		return status;
	}

	for (i=5; i<1000; i++) {
		times3[i+5] = times[i];
		for (j=0; j<20; j++) {
			dvalues3[i+5][j] = (double)((i * 1000) + j);
		}
	}
	tssIn3->times = times3;
	tssIn3->doubleProfileValues = (double *)dvalues3;
	tssIn3->numberValues = 1005;

	status = compareProfiles(ifltab, tssIn3, tssOut2, "Profile double to double, Loc 28");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tssIn3->pathname);
		zmessage(ifltab, "\ntestProfileIrreg failed!\nLoc 29\n");
		return status;
	}

		zstructFree(tssOut2);

		
	// -------------------------------------------------------------------

	//  Delete those values to restore original data set
	minsToDateTime(times[0] - MINS_IN_1_DAY, endDate, endTime, sizeof(endDate), sizeof(endTime));
	tssIn3->doubleProfileValues = (double *)dvalues3;
	tssIn3->numberValues = 0;

	status = ztsStore(ifltab, tssIn3, 1);
	stringCopy(mess, sizeof(mess), "testProfileIrreg Loc 30, regular double store status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tssIn3->pathname);
		return status;
	}


	//   Check
	
	minsToDateTime(times[0]-(5 * MINS_IN_1_DAY), startDate, startTime, sizeof(startDate), sizeof(startTime));
	minsToDateTime(times[tssIn1->numberValues-1], endDate, endTime, sizeof(endDate), sizeof(endTime));

	//  Write doubles, read doubles
	tssOut2 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);
	status = ztsRetrieve(ifltab, tssOut2, -1, 2, 1);
	stringCopy(mess, sizeof(mess), "testProfileIrreg Loc 32, regular double retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tssOut2->pathname);
		return status;
	}

	zmessage(ifltab, "\n\n******  FORCE ERRORS  *****\n");
	status = compareProfiles(ifltab, tssIn2, tssOut2, "Profile double to double, Loc 33");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tssIn2->pathname);
		zmessage(ifltab, "\ntestProfileIrreg failed!\nLoc 34\n");
//		return status;
	}

	zstructFree(tssOut2);


	// -------------------------------------------------------------------
	//  read before into data

	
	for (i=0; i<500; i++) {
		for (j=0; j<20; j++) {			
			dvalues3[i][j] = (double)zmissingFlagDouble();
		}
	}
	for (i=0; i<500; i++) {		
		for (j=0; j<20; j++) {
			dvalues3[i+500][j] = (double)(i * 1000) + j;
		}
	}

//////////////////////////////////////
	//////////////   this does not work!!!

	tss3 = zstructTsNew(pathname2);

	tss3->profileDepthsNumber = 20;
	tss3->numberValues = 1000;	
	tss3->numberValues = tss3->numberValues;
	tss3->doubleProfileDepths = ddepths;
	tss3->doubleProfileValues = (double *)dvalues3;
	//tss3->times = times;
	tss3->unitsProfileDepths = "feet";
	tss3->unitsProfileValues = "deg F";
	
	itime = times[0] - (500 * MINS_IN_1_DAY);
	minsToDateTime(itime, startDate, startTime, sizeof(startDate), sizeof(startTime));
	itime = times[tssIn1->numberValues-1] - (500 * MINS_IN_1_DAY);
	minsToDateTime(itime, endDate, endTime, sizeof(endDate), sizeof(endTime));

	//  read before into data
	tssOut2 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);
	status = ztsRetrieve(ifltab, tssOut2, 0, 2, 1);
	stringCopy(mess, sizeof(mess), "testProfileIrreg Loc 35, regular double retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tssOut2->pathname);
		return status;
	}

	status = compareProfiles(ifltab, tss3, tssOut2, "Profile double to double");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tss3->pathname);
		zmessage(ifltab, "\ntestProfileIrreg failed!\nLoc 36\n");
//		return status;
	}

	zstructFree(tssOut2);
	zstructFree(tss3);

	zmessage(ifltab, "******  Done Forcing Errors *****\n");


	// -------------------------------------------------------------------

	//  read middle of data to beyond data

	
	for (i=0; i<500; i++) {
		times3[i] = times[i+500];
		for (j=0; j<20; j++) {
			dvalues3[i][j] = (double)((i+500) * 1000) + j;
		}
	}


	
	itime = times[0] + (500 * MINS_IN_1_DAY);
	minsToDateTime(itime, startDate, startTime, sizeof(startDate), sizeof(startTime));
	itime = times[tssIn1->numberValues-1] + (500 * MINS_IN_1_DAY);
	minsToDateTime(itime, endDate, endTime, sizeof(endDate), sizeof(endTime));

	tss3 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);
	tss3->profileDepthsNumber = 20;
	tss3->numberValues = 500;	
	tss3->doubleProfileDepths = ddepths;
	tss3->doubleProfileValues = (double *)dvalues3;
	tss3->times = times3;
	tss3->unitsProfileDepths = "feet";
	tss3->unitsProfileValues = "deg F";
	tss3->type = "INST-VAL";


	//  read before into data
	tssOut2 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);
	status = ztsRetrieve(ifltab, tssOut2, 0, 2, 1);
	stringCopy(mess, sizeof(mess), "testProfileIrreg Loc 37, regular double retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tssOut2->pathname);
		return status;
	}
	
	status = compareProfiles(ifltab, tss3, tssOut2, "Profile double to double");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tss3->pathname);
		zmessage(ifltab, "\ntestProfileIrreg failed!\nLoc 38\n");
		return status;
	}

	zstructFree(tssOut2);
	zstructFree(tss3);


	// -------------------------------------------------------------------

	//  Now test getting doubles from floats
	minsToDateTime(times[0], startDate, startTime, sizeof(startDate), sizeof(startTime));
	minsToDateTime(times[tssIn1->numberValues-1], endDate, endTime, sizeof(endDate), sizeof(endTime));

	//  Write floats, read doubles
	tssOut2 = zstructTsNewTimes(pathname1, startDate, startTime, endDate, endTime);
	status = ztsRetrieve(ifltab, tssOut2, 0, 2, 1);
	stringCopy(mess, sizeof(mess), "testProfileIrreg Loc 41, regular float to double retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tssOut2->pathname);
		return status;
	}

	status = compareProfiles(ifltab, tssIn2, tssOut2, "Profile float to double, loc 42");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, tssIn2->pathname);
		zmessage(ifltab, "\ntestProfileIrreg failed!\nLoc 43\n");
		return status;
	}
	zstructFree(tssOut2);

	// -------------------------------------------------------------------


	zstructFree(tssOut1);

	// -------------------------------------------------------------------




	// -------------------------------------------------------------------

	//  All done

	zstructFree(tssIn1);
	zstructFree(tssIn3);
	zstructFree(tssIn2);
	

	printf("Completed testProfileIrreg  successfully!\n");

	return 0;
}

