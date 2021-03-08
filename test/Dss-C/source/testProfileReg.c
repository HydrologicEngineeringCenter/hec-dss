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





int testProfileReg(long long *ifltab)
{

	//  100 rows by 50 columns (curves)
	

	//  INPUT ONLY  - do not mix
	//   tssIn1 = floats,    tssIn2 = doubles
	zStructTimeSeries *tssIn1, *tssIn2;
	zStructTimeSeries *tssIn3, *tssIn4;
	//  OUT ONLY
	zStructTimeSeries *tssOut1, *tssOut2;
	zStructTimeSeries *tss3;

	float depths[20];
	float values[1000][20];
	double ddepths[20];
	double dvalues[1000][20];
	double dvalues3[3000][20];
	int times[1000];
	int itime;

	char mess[100];
	int i, j;
	int status;
	int nrows, ncols;
	char pathname1[] = "/a/b/Temperature-Depth/20Jun2020/1Day/Float Profile Test/";
	char pathname2[] = "/a/b/Temperature-Depth/20Jun2020/1Day/Double Profile Test/";
	int zero = 0;

	char startDate[20];
	char endDate[20];
	char startTime[20];
	char endTime[20];

	nrows = 100;
	ncols = 50;


	//  This function tests features not supported in DSS-6
	if (zgetVersion(ifltab) == 6) {
		return 0;
	}

	
	//  zcheck(long long *ifltab, const char* pathname, int statusWanted, int *istat)
	status = zcheck(ifltab, pathname1);
	if (zisError(status)) {
		//  An error code 
		return status;
	}
	if (status == STATUS_RECORD_FOUND) {
		zdelete(ifltab, pathname1);
	}
	status = zcheck(ifltab, pathname2);
	if (zisError(status)) {
		//  An error code 
		return status;
	}
	if (status == STATUS_RECORD_FOUND) {
		zdelete(ifltab, pathname2);
	}

	//  20 depth measurements, 3 meters apart with first at surface, last at 57 meters deep
	for (i=0; i<20; i++) {
		depths[i] = (float)(i);
		ddepths[i] = (double)depths[i];
	}

	//  We have 16 months of daily 8:00 am readings
	for (i=0; i<1000; i++) {
		times[i] = (25000 * 1440) + (i * 1440) + (8 * 60);
		for (j=0; j<20; j++) {
			//values[i][j] = (20. - (float)j) + 35.0 + sin((float)i);
			values[i][j] = (float)(i * 1000) + j;
			dvalues[i][j] = (double)values[i][j];
		}
	}

	minsToDateTime(times[0], startDate, startTime, sizeof(startDate), sizeof(startTime));
	minsToDateTime(times[999], endDate, endTime, sizeof(endDate), sizeof(endTime));
	tssIn1 = zstructTsNewTimes(pathname1, startDate, startTime, endDate, endTime);

	tssIn1->profileDepthsNumber = 20;
	tssIn1->numberValues = 1000;	
	tssIn1->floatProfileDepths = (float *)depths;
	tssIn1->floatProfileValues = (float *)values;	
	tssIn1->unitsProfileDepths = "feet";
	tssIn1->unitsProfileValues = "deg F";


	////////////////////////////////////////
	/////////////////////////////////////////
	///   FIX ME - BUILD A NEW STRUCT FUNCTION FOR PROFILES!!!

	tssIn2 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);

	tssIn2->profileDepthsNumber = 20;
	tssIn2->numberValues = 1000;	
	tssIn2->doubleProfileDepths = (double *)ddepths;
	tssIn2->doubleProfileValues = (double *)dvalues;
	tssIn2->unitsProfileDepths = "feet";
	tssIn2->unitsProfileValues = "deg F";

	status = ztsStore(ifltab, tssIn1, 0);
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 2, regular float store status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;


	minsToDateTime(times[0], startDate, startTime, sizeof(startDate), sizeof(startTime));
	minsToDateTime(times[999], endDate, endTime, sizeof(endDate), sizeof(endTime));

	//  Write floats, read floats
	tssOut1 = zstructTsNewTimes(pathname1, startDate, startTime, endDate, endTime);
	status = ztsRetrieve(ifltab, tssOut1, 0, 1, 1);
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 4, regular retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = compareProfiles(ifltab, tssIn1, tssOut1, "Profile Float to Float, Loc 5");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, "\ntestProfileReg failed!\nLoc 6\n");
		return status;
	}

	zstructFree(tssOut1);

	// -------------------------------------------------------------------
	

	//  Now test getting doubles from doubles
	
	status = ztsStore(ifltab, tssIn2, 0);
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 11, regular double store status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;


	minsToDateTime(times[0], startDate, startTime, sizeof(startDate), sizeof(startTime));
	minsToDateTime(times[tssIn1->numberValues-1], endDate, endTime, sizeof(endDate), sizeof(endTime));

	//  Write doubles, read doubles
	tssOut2 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);
	status = ztsRetrieve(ifltab, tssOut2, 0, 2, 1);
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 13, regular double retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = compareProfiles(ifltab, tssIn2, tssOut2, "Profile double to double, Loc 14");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, "\ntestProfileReg failed!\nLoc 15\n");
		return status;
	}

	zstructFree(tssOut2);


	//  -----------------------------------------------------------------

	//  write a little bit of new data into the existing record

	for (i=0; i<10; i++) {
		for (j=0; j<20; j++) {
			//values[i][j] = (20. - (float)j) + 35.0 + sin((float)i);
			//values[i][j] = (float)(i * 1000) + j;
			dvalues3[i][j] = (double)((i * 10) + j);
		}
	}

	minsToDateTime(times[0]-(5*1440), startDate, startTime, sizeof(startDate), sizeof(startTime));
	minsToDateTime(times[0]+(4*1440), endDate, endTime, sizeof(endDate), sizeof(endTime));

	tssIn4 = zstructTsNewTimes(pathname2, startDate, startTime,"", "");
	

	tssIn4->profileDepthsNumber = 20;
	tssIn4->numberValues = 10;	
	tssIn4->doubleProfileDepths = (double *)ddepths;
	tssIn4->doubleProfileValues = (double *)dvalues3;
	tssIn4->unitsProfileDepths = "feet";
	tssIn4->unitsProfileValues = "deg F";

	minsToDateTime(times[0]-(5*1440), startDate, startTime, sizeof(startDate), sizeof(startTime));
	minsToDateTime(times[0]+(4*1440), endDate, endTime, sizeof(endDate), sizeof(endTime));
	status = ztsStore(ifltab, tssIn4, 0);
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 21, regular double store status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;


	tssOut2 = zstructTsNewTimes(pathname2, startDate, "2400", endDate, "2400");
	status = ztsRetrieve(ifltab, tssOut2, 0, 2, 1);
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 23, regular double retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = compareProfiles(ifltab, tssIn4, tssOut2, "Profile double to double, Loc 24");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, "\ntestProfileReg failed!\nLoc 25\n");
		return status;
	}

	zstructFree(tssOut2);

	//   Now full data set

	minsToDateTime(times[tssIn1->numberValues-1], endDate, endTime, sizeof(endDate), sizeof(endTime));
	tssOut2 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);
	status = ztsRetrieve(ifltab, tssOut2, 0, 2, 1);
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 27, regular double retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	for (i=5; i<1000; i++) {
		for (j=0; j<20; j++) {
			dvalues3[i+5][j] = (double)((i * 1000) + j);
		}
	}
	tssIn4->doubleProfileValues = (double *)dvalues3;
	tssIn4->numberValues = 1005;

	status = compareProfiles(ifltab, tssIn4, tssOut2, "Profile double to double, Loc 28");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, "\ntestProfileReg failed!\nLoc 29\n");
		return status;
	}

		zstructFree(tssOut2);

		
	// -------------------------------------------------------------------


	//   Now restore original data set
	minsToDateTime(times[0]-(5*1440), startDate, startTime, sizeof(startDate), sizeof(startTime));
	zstructFree(tssIn4);
	tssIn4 = zstructTsNewTimes(pathname2, startDate, startTime,"", "");
	
	minsToDateTime(times[0] -1440, endDate, endTime, sizeof(endDate), sizeof(endTime));
	for (i=0; i<5; i++) {
		for (j=0; j<20; j++) {
			dvalues3[i][j] = zmissingFlagDouble();
		}
	}
	tssIn4->doubleProfileValues = (double *)dvalues3;
	tssIn4->numberValues = 5;
	tssIn4->profileDepthsNumber = 20;
	tssIn4->doubleProfileDepths = (double *)ddepths;
	tssIn4->unitsProfileDepths = "feet";
	tssIn4->unitsProfileValues = "deg F";

	status = ztsStore(ifltab, tssIn4, 0);
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 30, regular double store status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = ztsStore(ifltab, tssIn2, 0);
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 31, regular double store status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;


	//   Check

	minsToDateTime(times[0]-(5*1440), startDate, startTime, sizeof(startDate), sizeof(startTime));
	minsToDateTime(times[tssIn1->numberValues-1], endDate, endTime, sizeof(endDate), sizeof(endTime));

	//  Write doubles, read doubles
	tssOut2 = zstructTsNewTimes(pathname2, startDate, "2400", endDate, "2400");
	status = ztsRetrieve(ifltab, tssOut2, -1, 2, 1);
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 32, regular double retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = compareProfiles(ifltab, tssIn2, tssOut2, "Profile double to double, Loc 29d");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, "\ntestProfileReg failed!\nLoc 33\n");
		return status;
	}

	zstructFree(tssOut2);
	tssOut2 = 0;


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

	itime = times[0] - (500 * 1440);
	minsToDateTime(itime, startDate, startTime, sizeof(startDate), sizeof(startTime));
	itime = times[tssIn1->numberValues-1] - (500 * 1440);
	minsToDateTime(itime, endDate, endTime, sizeof(endDate), sizeof(endTime));

	tss3 = zstructTsNewTimes(pathname2, startDate, startTime, "", "");

	tss3->profileDepthsNumber = 20;
	tss3->numberValues = 1000;	
	tss3->numberValues = tss3->numberValues;
	tss3->doubleProfileDepths = ddepths;
	tss3->doubleProfileValues = (double *)dvalues3;
	//tss3->times = times;
	tss3->unitsProfileDepths = "feet";
	tss3->unitsProfileValues = "deg F";
	ztsProcessTimes(ifltab, tss3, 1);
	//zset7("mlvl", " ", 15);

	//  read before into data
	tssOut2 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);
	status = ztsRetrieve(ifltab, tssOut2, 0, 2, 1);
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 35, regular double retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = compareProfiles(ifltab, tss3, tssOut2, "Profile double to double");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, "\ntestProfileReg failed!\nLoc 36\n");
		return status;
	}

	zstructFree(tssOut2);
	zstructFree(tss3);

	// -------------------------------------------------------------------

	//  read middle of data to beyond data

	
	for (i=0; i<500; i++) {		
		for (j=0; j<20; j++) {
			dvalues3[i][j] = (double)((i+500) * 1000) + j;
		}
	}
	for (i=500; i<1000; i++) {
		for (j=0; j<20; j++) {			
			dvalues3[i][j] = (double)zmissingFlagDouble();
		}
	}

	itime = times[0] + (500 * 1440);
	minsToDateTime(itime, startDate, startTime, sizeof(startDate), sizeof(startTime));
	itime = times[tssIn1->numberValues-1] + (500 * 1440);
	minsToDateTime(itime, endDate, endTime, sizeof(endDate), sizeof(endTime));

	tss3 = zstructTsNewTimes(pathname2, startDate, startTime, "", "");

	tss3->profileDepthsNumber = 20;
	tss3->numberValues = 1000;	
	tss3->numberValues = tss3->numberValues;
	tss3->doubleProfileDepths = ddepths;
	tss3->doubleProfileValues = (double *)dvalues3;
	//tss3->times = times;
	tss3->unitsProfileDepths = "feet";
	tss3->unitsProfileValues = "deg F";
	ztsProcessTimes(ifltab, tss3, 1);



	//  read before into data
	tssOut2 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);
	status = ztsRetrieve(ifltab, tssOut2, 0, 2, 1);
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 37, regular double retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = compareProfiles(ifltab, tss3, tssOut2, "Profile double to double");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, "\ntestProfileReg failed!\nLoc 38\n");
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
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 41, regular float to double retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = compareProfiles(ifltab, tssIn2, tssOut2, "Profile float to double, loc 42");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, "\ntestProfileReg failed!\nLoc 43\n");
		return status;
	}
	zstructFree(tssOut2);

	// -------------------------------------------------------------------

	//  Floats from doubles
	minsToDateTime(times[0], startDate, startTime, sizeof(startDate), sizeof(startTime));
	minsToDateTime(times[tssIn1->numberValues-1], endDate, endTime, sizeof(endDate), sizeof(endTime));

	//  Write doubles, read floats
	tssOut1 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);
	status = ztsRetrieve(ifltab, tssOut1, 0, 0, 1);
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 51, regular double to float retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = compareProfiles(ifltab, tssIn1, tssOut1, "Profile float to double, Loc 52");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, "\ntestProfileReg failed!\nLoc 53\n");
		return status;
	}
	zstructFree(tssOut1);

	// -------------------------------------------------------------------


	//  Now try different times - no trim
	
	itime = times[0] - (365 * 1440);
	minsToDateTime(itime, startDate, startTime, sizeof(startDate), sizeof(startTime));
	itime = times[tssIn1->numberValues-1] + (365 * 1440);
	minsToDateTime(itime, endDate, endTime, sizeof(endDate), sizeof(endTime));
	
	//  We have 16 months of daily 8:00 am readings
	for (i=0; i<365; i++) {
		for (j=0; j<20; j++) {			
			dvalues3[i][j] = (double)zmissingFlagDouble();
		}
	}
	for (i=0; i<1000; i++) {		
		for (j=0; j<20; j++) {
			dvalues3[i+365][j] = (double)(i * 1000) + j;
		}
	}
	for (i=0; i<365; i++) {
		for (j=0; j<20; j++) {			
			dvalues3[i+365+1000][j] = (double)zmissingFlagDouble();
		}
	}

	tss3 = zstructTsNewTimes(pathname2, startDate, startTime, "", "");

	tss3->profileDepthsNumber = 20;
	tss3->numberValues = 1000 + (2 * 365);	
	tss3->numberValues = tss3->numberValues;
	tss3->doubleProfileDepths = ddepths;
	tss3->doubleProfileValues = (double *)dvalues3;
	//tss3->times = times;
	tss3->unitsProfileDepths = "feet";
	tss3->unitsProfileValues = "deg F";
	ztsProcessTimes(ifltab, tss3, 1);

	//  Write doubles, read doubles
	tssOut2 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);
	status = ztsRetrieve(ifltab, tssOut2, 0, 2, 1);
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 61, regular double retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = compareProfiles(ifltab, tss3, tssOut2, "Profile double to double no trim, loc 62");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, "\ntestProfileReg failed!\nLoc 63\n");
		return status;
	}

	zstructFree(tssOut2);

	// -------------------------------------------------------------------


	//  Now try different times - trim
		
	itime = times[0] - (365 * 1440);
	minsToDateTime(itime, startDate, startTime, sizeof(startDate), sizeof(startTime));
	itime = times[tssIn1->numberValues-1] + (365 * 1440);
	minsToDateTime(itime, endDate, endTime, sizeof(endDate), sizeof(endTime));

	//  Write doubles, read doubles
	tssOut2 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);
	status = ztsRetrieve(ifltab, tssOut2, -1, 2, 1);
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 71, regular double retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = compareProfiles(ifltab, tssIn2, tssOut2, "Profile double to double trim, Loc 72");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, "\ntestProfileReg failed!\nLoc 73\n");
		return status;
	}

	zstructFree(tssOut2);

	// -------------------------------------------------------------------


	//   Now write a data set so 
	//  values ...   missing ... values
	//  and read similar to above
	itime = times[0] + (2000 * 1440);
	minsToDateTime(itime, startDate, startTime, sizeof(startDate), sizeof(startTime));
	itime = times[0] + (2999 * 1440);
	minsToDateTime(itime, endDate, endTime, sizeof(endDate), sizeof(endTime));

	tssIn3 = zstructTsNewTimes(pathname2, startDate, startTime, "", "");

	tssIn3->profileDepthsNumber = 20;
	tssIn3->numberValues = 1000;	
	tssIn3->doubleProfileDepths = (double *)ddepths;
	tssIn3->doubleProfileValues = (double *)dvalues;
	tssIn3->unitsProfileDepths = "feet";
	tssIn3->unitsProfileValues = "deg F";



	status = ztsStore(ifltab, tssIn3, 0);
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 81, double store status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;


	itime = times[0] - (365 * 1440);
	minsToDateTime(itime, startDate, startTime, sizeof(startDate), sizeof(startTime));
	itime = times[0] + (3500 * 1440);
	minsToDateTime(itime, endDate, endTime, sizeof(endDate), sizeof(endTime));

	//  Write doubles, read doubles
	tssOut2 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);
	status = ztsRetrieve(ifltab, tssOut2, -1, 2, 1);
	stringCopy(mess, sizeof(mess), "testProfileReg Loc 83, regular double retrieve status ", sizeof(mess)); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;


	minsToDateTime(times[0], startDate, startTime, sizeof(startDate), sizeof(startTime));
	itime = times[0] + (2999 * 1440);
	minsToDateTime(itime, endDate, endTime, sizeof(endDate), sizeof(endTime));
	
	// 
	for (i=0; i<1000; i++) {		
		for (j=0; j<20; j++) {
			dvalues3[i][j] = (double)(i * 1000) + j;
		}
	}
	for (i=1000; i<2000; i++) {
		for (j=0; j<20; j++) {			
			dvalues3[i][j] = (double)zmissingFlagDouble();
		}
	}
	for (i=2000; i<3000; i++) {		
		for (j=0; j<20; j++) {
			dvalues3[i][j] = (double)((i - 2000) * 1000) + j;
		}
	}

	zstructFree(tss3);
	tss3 = zstructTsNewTimes(pathname2, startDate, startTime, endDate, endTime);

	tss3->profileDepthsNumber = 20;
	tss3->numberValues = 3000;	
	tss3->numberValues = tss3->numberValues;
	tss3->doubleProfileDepths = ddepths;
	tss3->doubleProfileValues = (double *)dvalues3;
	//tss3->times = times;
	tss3->unitsProfileDepths = "feet";
	tss3->unitsProfileValues = "deg F";
	ztsProcessTimes(ifltab, tss3, 1);

	status = compareProfiles(ifltab, tss3, tssOut2, "Profile double to double trim, loc 84");
	if (status != STATUS_OKAY) {
		zmessage(ifltab, "\ntestProfileReg failed!\nLoc 85\n");
		return status;
	}

	zstructFree(tss3);
	zstructFree(tssOut2);


	// -------------------------------------------------------------------

	//  All done

	zstructFree(tssIn1);
	zstructFree(tssIn2);
	zstructFree(tssIn3);
	zstructFree(tssIn4);
	

	printf("Completed testProfileReg  successfully!\n");

	return 0;
}


