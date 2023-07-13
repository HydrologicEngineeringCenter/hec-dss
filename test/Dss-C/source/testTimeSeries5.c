#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "standardIntervals.h"
#include "TestDssC.h"


	

int testTimeSeries5(long long *ifltab)
{
	zStructTimeSeries *tss1, *tss2;
	float fvalues[200];
	int i;
	int status;
	int zero = 0;
	int jul;
	int min;
	int number1;
	int number2;
	char cdate[13];
	char ctime[12];
	char cstartDate[13];
	char cstartTime[12];
	char cdum[1];

	jul = 40000;
	min = 0;

	//zset("mlvl", "", 2);

	for (i=0; i<5000; i++) {

		min++;
		if (min > MINS_IN_1_DAY) {
			min = 1;
			jul++;
		}

		julianToDate(jul, 4, cdate, sizeof(cdate));
		minutesToHourMin(min, ctime, sizeof(ctime));
		fvalues[0] = (float)i;
		if (i == 0) {
			julianToDate(jul, 4, cstartDate, sizeof(cstartDate));
			minutesToHourMin(min, cstartTime, sizeof(cstartTime));
		}

		tss1 = zstructTsNewRegFloats("/Basin/Location/Flow//1Minute/Test Dead Space/", fvalues, 1, cdate, ctime, "cfs", "Inst-Val");
		status = ztsStore(ifltab, tss1, 0);
		if (zcheckStatus(ifltab, status, 1, "Fail in testTimeSeries5 Loc 1, store status ")) return status; 

		tss2 = zstructTsNewTimes("/Basin/Location/Flow//1Minute/Test Dead Space/", cstartDate, cstartTime, cdate, ctime);
		status = ztsRetrieve(ifltab, tss2, -1, 0, 0);
		if (zcheckStatus(ifltab, status, 1, "Fail in testTimeSeries5 Loc 2, retrieve status ")) return status; 

		//status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testztsStruct1, Location 3");
		//if (status) return status;

		zstructFree(tss1);
		zstructFree(tss2);

		number1 = zinquire(ifltab, "dsiz");
		number2 = zinquire(ifltab, "fsiz");

		printf("Iteration %d, file size:  %d,  dead space:  %d\n", i, number2, number1);

		zero = 0;

	}


	return 0; 
}

