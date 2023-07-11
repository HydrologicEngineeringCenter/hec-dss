#include "stdio.h"
#include "string.h"

#include "heclib.h"
#include "hecdssInternal.h"
#include "zStructTimeSeries.h"
#include "TestDssC.h"



	//  Unit test for ztsProcessTimes

int testZtsProcessTimes(long long *ifltab)
{
	char path1[100];	
	char path3[100];
	char mess[100];
	int status;

	zStructTimeSeries *tss1;
	ztsTimeWindow *timeWindow1;
	ztsTimeWindow timeWindow2;

	//   /A/B/C/01JAN2010/1DAY/F/  (no start date, end date)
	//   /A/B/C/17FEB2010 - 25MAR2015/1DAY/F/  (no start date, end date)
	//   /A/B/C/01JAN2010/1DAY/F/  with start date, end date
	//   /A/B/C/17FEB2010 - 25MAR2015/1DAY/F/  with start date, end date
	//   /A/B/C/01JAN2010/1DAY/F/  with start date, number values  retrieve
	//   /A/B/C/01JAN2010/1DAY/F/  with start date, number values  store
	//
	//   Ditto with irregular interval (~1Day)
	zStructTimeSeries *tss2;
	//   /A/B/C/01JAN2010/1DAY/F/  (no start date, end date)
	clearztw(&timeWindow2);

	stringCopy(path1,  99, "/A/B/C/01JAN2010/1DAY/F/", 99);	
	tss1 = zstructTsNew(path1);
	ztsProcessTimes(ifltab, tss1, 0);

	timeWindow1 = tss1->timeWindow;
	
	timeWindow2.startJulian = 40178;
	timeWindow2.startTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.endJulian = 40542;
	timeWindow2.endTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.intervalSeconds = SECS_IN_1_DAY;
	timeWindow2.numberValues = 365;
	timeWindow2.blockSize = 3;
	timeWindow2.startBlockJulian = 40178;
	timeWindow2.endBlockJulian = 40178;
	if (compareZtw(timeWindow1, &timeWindow2, path1, "ztsProcessTimes Loc 10" )) return -1;

	
	stringCopy(path3,  99, "/A/B/C/01JAN2010/1Day/F/", 99);
	stringCopy(mess, sizeof(mess), "ztsProcessTimes Loc 11, pathnames ", sizeof(mess)); 
	checkstring_(tss1->pathname, path3, mess, &status, 
				  strlen(tss1->pathname), strlen(path3), strlen(mess));
	if (status != STATUS_OKAY) return -1;


	//   /A/B/C/17FEB2010 - 25MAR2015/1DAY/F/  (no start date, end date)
	clearztw(&timeWindow2);

	zstructFree(tss1);

	stringCopy(path1,  99, "/A/B/C/17FEB2010 - 25MAR2015/1DAY/F/", 99);	
	tss1 = zstructTsNew(path1);
	ztsProcessTimes(ifltab, tss1, 0);
	timeWindow1 = tss1->timeWindow;

	timeWindow2.startJulian = 40225;
	timeWindow2.startTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.endJulian = 42087;
	timeWindow2.endTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.intervalSeconds = SECS_IN_1_DAY;
	timeWindow2.numberValues = 1863;
	timeWindow2.blockSize = 3;
	timeWindow2.startBlockJulian = 40178;
	timeWindow2.endBlockJulian = 42004;
	if (compareZtw(timeWindow1, &timeWindow2, path1, "ztsProcessTimes Loc 20" )) return -1;

	//   /A/B/C/17FEB2010 - 25MAR2015/1DAY/F/  (no start date, end date)
	clearztw(&timeWindow2);

	zstructFree(tss1);
	stringCopy(path1,  99, "//SACRAMENTO/TEMP-MAX/01JAN1877 - 01JAN2009/1DAY/OBS/", 99);	
	tss1 = zstructTsNew(path1);
	//////////////////////////
	//tss1->startJulianDate = -8399;
	//tss1->startTimeSeconds = SECS_IN_1_MINUTE;
//	ztsMessTimeWindow(ifltab, 0, tss1);
	///////////////////////////
	ztsProcessTimes(ifltab, tss1, 0);
	timeWindow1 = tss1->timeWindow;
//	ztsMessTimeWindow(ifltab, 0, tss1);

	timeWindow2.startJulian = -8399;
	timeWindow2.startTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.endJulian = 40177;
	timeWindow2.endTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.intervalSeconds = SECS_IN_1_DAY;
	timeWindow2.numberValues = 48577;
	timeWindow2.blockSize = 3;
	timeWindow2.startBlockJulian = -8399;
	timeWindow2.endBlockJulian = 39813;
	if (compareZtw(timeWindow1, &timeWindow2, path1, "ztsProcessTimes Loc 25" )) return -1;	


	//   /A/B/C/01JAN2010/1DAY/F/  with start date, end date
	clearztw(&timeWindow2);

	zstructFree(tss1);
	stringCopy(path1,  99, "/A/B/C/D/1DAY/F/", 99);	
	tss1 = zstructTsNewTimes(path1,
					"17FEB2010", "2400",
					"25MAR2015", "2400");
	ztsProcessTimes(ifltab, tss1, 0);
	timeWindow1 = tss1->timeWindow;

	timeWindow2.startJulian = 40225;
	timeWindow2.startTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.endJulian = 42087;
	timeWindow2.endTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.intervalSeconds = SECS_IN_1_DAY;
	timeWindow2.numberValues = 1863;
	timeWindow2.blockSize = 3;
	timeWindow2.startBlockJulian = 40178;
	timeWindow2.endBlockJulian = 42004;
	if (compareZtw(timeWindow1, &timeWindow2, path1, "ztsProcessTimes Loc 30" )) return -1;
	

	//   /A/B/C/17FEB2010 - 25MAR2015/1DAY/F/  with start date, end date

	clearztw(&timeWindow2);

	zstructFree(tss1);
	stringCopy(path1,  99, "/A/B/C/17APR2009 - 25SEP2009/1DAY/F/", 99);	
	tss1 = zstructTsNewTimes(path1,
					"17FEB2010", "0700",
					"25MAR2015", "1200");
	ztsProcessTimes(ifltab, tss1, 0);
	timeWindow1 = tss1->timeWindow;

	//  Note - should return with standard times, not offset times supplied
	timeWindow2.startJulian = 40225;
	timeWindow2.startTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.endJulian = 42087;
	timeWindow2.endTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.intervalSeconds = SECS_IN_1_DAY;
	//  Offset is seconds after the standard time
	timeWindow2.timeOffsetSeconds = 7*SECS_IN_1_HOUR;
	timeWindow2.numberValues = 1863;
	timeWindow2.blockSize = 3;
	timeWindow2.startBlockJulian = 40178;
	timeWindow2.endBlockJulian = 42004;
	if (compareZtw(timeWindow1, &timeWindow2, path1, "ztsProcessTimes Loc 40" )) return -1;	

	//   /A/B/C/01JAN2010/1DAY/F/  with start date, number values  retrieve

	clearztw(&timeWindow2);
	zstructFree(tss1);
	stringCopy(path1,  99, "/A/B/C/D/1DAY/F/", 99);	
	tss1 = zstructTsNewTimes(path1, 
					"17FEB2010", "2400",
					"", "");
	tss1->numberValues = 50;
	ztsProcessTimes(ifltab, tss1, 0);
	timeWindow1 = tss1->timeWindow;

	timeWindow2.startJulian = 40225;
	timeWindow2.startTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.endJulian = 40274;
	timeWindow2.endTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.intervalSeconds = SECS_IN_1_DAY;
	timeWindow2.numberValues = 50;
	timeWindow2.blockSize = 3;
	timeWindow2.startBlockJulian = 40178;
	timeWindow2.endBlockJulian = 40178;
	if (compareZtw(timeWindow1, &timeWindow2, path1, "ztsProcessTimes Loc 50" )) return -1;
	

	//   /A/B/C/01JAN2010/17APR2009 - 25SEP2009/F/  with start date, number values  retrieve

	clearztw(&timeWindow2);
	zstructFree(tss1);
	stringCopy(path1,  99, "/A/B/C/17APR2009 - 25SEP2009/1DAY/F/", 99);	
	tss1 = zstructTsNewTimes(path1, 
					"17FEB2010", "2400",
					"", "");
	tss1->numberValues = 50;
	ztsProcessTimes(ifltab, tss1, 1);
	timeWindow1 = tss1->timeWindow;

	timeWindow2.startJulian = 40225;
	timeWindow2.startTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.endJulian = 40274;
	timeWindow2.endTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.intervalSeconds = SECS_IN_1_DAY;
	timeWindow2.numberValues = 50;
	timeWindow2.blockSize = 3;
	timeWindow2.startBlockJulian = 40178;
	timeWindow2.endBlockJulian = 40178;
	if (compareZtw(timeWindow1, &timeWindow2, path1, "ztsProcessTimes Loc 55" )) return -1;
	
	//   /A/B/C/01JAN2010/1DAY/F/  with start date, number values  store

	clearztw(&timeWindow2);
	zstructFree(tss1);
	stringCopy(path1,  99, "/A/B/C/D/1DAY/F/", 99);	
	tss1 = zstructTsNewTimes(path1, 
					"17FEB2010", "2400",
					"", "");
	tss1->numberValues = 50;
	ztsProcessTimes(ifltab, tss1, 1);
	timeWindow1 = tss1->timeWindow;

	timeWindow2.startJulian = 40225;
	timeWindow2.startTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.endJulian = 40274;
	timeWindow2.endTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.intervalSeconds = SECS_IN_1_DAY;
	timeWindow2.numberValues = 50;
	timeWindow2.blockSize = 3;
	timeWindow2.startBlockJulian = 40178;
	timeWindow2.endBlockJulian = 40178;
	if (compareZtw(timeWindow1, &timeWindow2, path1, "ztsProcessTimes Loc 60" )) return -1;
	

	//  Irregular Interval data


	//   /A/B/C/01JAN2010/~1Day/F/  (no start date, end date)
	clearztw(&timeWindow2);

	zstructFree(tss1);
	stringCopy(path1,  99, "/A/B/C/01JAN2010/~1Day/F/", 99);	
	tss1 = zstructTsNew(path1);
	ztsProcessTimes(ifltab, tss1, 0);
	timeWindow1 = tss1->timeWindow;

	timeWindow2.startJulian = 40178;
	timeWindow2.startTimeSeconds = 1;
	timeWindow2.endJulian = 40542;
	timeWindow2.endTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.intervalSeconds = -3;
	timeWindow2.numberValues = 0;
	timeWindow2.blockSize = 3;
	timeWindow2.startBlockJulian = 40178;
	timeWindow2.endBlockJulian = 40178;
	if (compareZtw(timeWindow1, &timeWindow2, path1, "ztsProcessTimes Loc 110" )) return -1;
	

	//   /A/B/C/17FEB2010 - 25MAR2015/~1Day/F/  (no start date, end date)
	clearztw(&timeWindow2);
	zstructFree(tss1);
	stringCopy(path1,  99, "/A/B/C/17FEB2010 - 25MAR2015/~1Day/F/", 99);	
	tss1 = zstructTsNew(path1);
	ztsProcessTimes(ifltab, tss1, 0);
	timeWindow1 = tss1->timeWindow;

	timeWindow2.startJulian = 40225;
	timeWindow2.startTimeSeconds = 1;
	timeWindow2.endJulian = 42087;
	timeWindow2.endTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.intervalSeconds = -3;
	timeWindow2.numberValues = 0;
	timeWindow2.blockSize = 3;
	timeWindow2.startBlockJulian = 40178;
	timeWindow2.endBlockJulian = 42004;
	if (compareZtw(timeWindow1, &timeWindow2, path1, "ztsProcessTimes Loc 120" )) return -1;
	


	//   /A/B/C/01JAN2010/~1Day/F/  with start date, end date
	clearztw(&timeWindow2);
	zstructFree(tss1);
	stringCopy(path1,  99, "/A/B/C/D/~1Day/F/", 99);	
	tss1 = zstructTsNewTimes(path1, 
					"17FEB2010", "2400",
					"25MAR2015", "2400");
	ztsProcessTimes(ifltab, tss1, 0);
	timeWindow1 = tss1->timeWindow;

	timeWindow2.startJulian = 40225;
	timeWindow2.startTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.endJulian = 42087;
	timeWindow2.endTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.intervalSeconds = -3;
	timeWindow2.numberValues = 0;
	timeWindow2.blockSize = 3;
	timeWindow2.startBlockJulian = 40178;
	timeWindow2.endBlockJulian = 42004;
	if (compareZtw(timeWindow1, &timeWindow2, path1, "ztsProcessTimes Loc 130" )) return -1;
	

	//   /A/B/C/17FEB2010 - 25MAR2015/~1Day/F/  with start date, end date
	clearztw(&timeWindow2);
	zstructFree(tss1);
	stringCopy(path1,  99, "/A/B/C/17APR2009 - 25SEP2009/~1Day/F/", 99);	
	tss1 = zstructTsNewTimes(path1, 
					"17FEB2010", "2400",
					"25MAR2015", "2400");
	ztsProcessTimes(ifltab, tss1, 0);
	timeWindow1 = tss1->timeWindow;

	timeWindow2.startJulian = 40225;
	timeWindow2.startTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.endJulian = 42087;
	timeWindow2.endTimeSeconds = SECS_IN_1_DAY;
	timeWindow2.intervalSeconds = -3;
	timeWindow2.numberValues = 0;
	timeWindow2.blockSize = 3;
	timeWindow2.startBlockJulian = 40178;
	timeWindow2.endBlockJulian = 42004;
	if (compareZtw(timeWindow1, &timeWindow2, path1, "ztsProcessTimes Loc 140" )) return -1;
	

	zstructFree(tss1);

	return 0; 
}

void clearztw(ztsTimeWindow *timeWindow)
{
	timeWindow->startJulian = UNDEFINED_TIME;
	timeWindow->startTimeSeconds = -1;
	timeWindow->endJulian = UNDEFINED_TIME;
	timeWindow->endTimeSeconds = -1;
	timeWindow->intervalSeconds = 0;
	timeWindow->timeOffsetSeconds = 0;
	timeWindow->numberValues = 0;
	timeWindow->blockSize = 0;
	timeWindow->startBlockJulian = 0;
	timeWindow->endBlockJulian = 0;
}

int compareZtw(ztsTimeWindow *timeWindow1, ztsTimeWindow *timeWindow2, char *path, char *mess)
{
	if (compareZtwItem(timeWindow1->startJulian, timeWindow2->startJulian, "timeWindow->startJulian", path, mess)) {
		return 1;
	}
	if (compareZtwItem(timeWindow1->startTimeSeconds, timeWindow2->startTimeSeconds, "timeWindow->startTimeSeconds", path, mess)) {
		return 1;
	}
	if (compareZtwItem(timeWindow1->endJulian, timeWindow2->endJulian, "timeWindow->endJulian", path, mess)) {
		return 1;
	}
	if (compareZtwItem(timeWindow1->endTimeSeconds, timeWindow2->endTimeSeconds, "timeWindow->endTimeSeconds", path, mess)) {
		return 1;
	}
	if (compareZtwItem(timeWindow1->intervalSeconds, timeWindow2->intervalSeconds, "timeWindow->intervalSeconds", path, mess)) {
		return 1;
	}
	if (compareZtwItem(timeWindow1->timeOffsetSeconds, timeWindow2->timeOffsetSeconds, "timeWindow->timeOffsetSeconds", path, mess)) {
		return 1;
	}
	if (compareZtwItem(timeWindow1->numberValues, timeWindow2->numberValues, "timeWindow->numberValues", path, mess)) {
		return 1;
	}
	if (compareZtwItem(timeWindow1->blockSize, timeWindow2->blockSize, "timeWindow->intervablockSizelSeconds", path, mess)) {
		return 1;
	}
	if (compareZtwItem(timeWindow1->startBlockJulian, timeWindow2->startBlockJulian, "timeWindow->startBlockJulian", path, mess)) {
		return 1;
	}
	if (compareZtwItem(timeWindow1->endBlockJulian, timeWindow2->endBlockJulian, "timeWindow->endBlockJulian", path, mess)) {
		return 1;
	}

	return 0;
}

int compareZtwItem(int one, int two, const char *name, char *path, char *mess)
{
	if (one != two) {
		printf ("%s failed, first = %d, second = %d\n", name, one, two);
		printf ("failed for pathname %s\n", path);
		printf ("%s\n", mess);
		return 1;
	}
	return 0;
}
