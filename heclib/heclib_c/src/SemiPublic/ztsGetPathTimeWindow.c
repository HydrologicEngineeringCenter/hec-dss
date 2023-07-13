#include <string.h>
#include <ctype.h>

#include "hecdssInternal.h"
#include "heclib.h"


/**
*  Function:	ztsGetPathTimeWindow
*
*  Use:			Semi Public
*
*  Description:	Gets the time window for the given path.  The path may have a time window for the D part,
*					e.g., .../12JUL2002 - 13SEP2004/1DAY/...  Adjusts E part if not standard.
*
*  Declaration: int ztsGetPathTimeWindow(int version, char* pathname, size_t sizeofPathname, ztsTimeWindow *timeWindow);
*
*  Parameters:	int version
*					The DSS file version (6 or 7).  Different versions have different intervals.  Use zgetVersion(ifltab).
*
*				int blockSize
*					A flag that tells the size of the block.  To decrement by one, set this to negative
*
*				char* pathname
*					The pathname to evaluate.  The E part will be changed to standard one, if not standard.
*
*				size_t sizeofPathname
*					The size of the pathname char array (not length), so that the E part can be changed if not standard
*
*				ztsTimeWindow *timeWindow
*					The time window struct to contain time window from path
*				
*
*	Returns:	int status 
*					Returns 1 if time series and dates obtained from path (/12JUL2002 - 13SEP2004/1Day/)
*					Return 0 if time series, but no dates  (/01JAN2002/1Day)
*					Returns STATUS_NOT_OKAY (-1) if not time series
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/



int ztsGetPathTimeWindow(int version, char* pathname, size_t sizeofPathname, ztsTimeWindow *timeWindow) 
{
	int numberValues;
	int len;
	int boolIrreg;
	int operation;
	int startBlockJulian;
	int blockSize;
	int boolAdjustEnd;
	int boolEndDateGiven;
	int intervalSeconds;
	int startSecondsAfterMidnight;
	int startJulian;
	int endJulian;
	int julian;
	int status;
	int ipos;
	const char *ptr;
	char dPart[MAX_PART_SIZE];  //  time series e parts will be short ( < 10 chars)
	char ePart[MAX_PART_SIZE];
	char date[20];
	char c;

	//  Set the times undefined if we should run into an error
	blockSize = 0;
	//  Get the E part (time interval) from the pathname
	len = zpathnameGetPart (pathname, 5, ePart, sizeof(ePart));
	if (len < 4) {
		//  Not a valid length for a time series interval		
		return STATUS_NOT_OKAY;
	}

	//  Get the time interval 
	operation = EPART_TO_SECONDS_TO_EPART;
	boolIrreg = ztsGetStandardInterval(version, &intervalSeconds, ePart, sizeof(ePart), &operation);
	timeWindow->intervalSeconds = intervalSeconds;
	 if (boolIrreg == 0) {
		 //  Regular Interval	
		 if (intervalSeconds < SECS_IN_1_DAY) {
			 startSecondsAfterMidnight = intervalSeconds;
		 }
		 else {
			 startSecondsAfterMidnight = SECS_IN_1_DAY;
		 }
		//  Put the standardized E part (back) into the pathname
		zpathnameSetPart (pathname, sizeofPathname, ePart, 5);
	}
	 else if (boolIrreg == 1) {
		//  Irregular interval data		
		timeWindow->blockSize = -intervalSeconds;
		startSecondsAfterMidnight = 1;
	}
	else {
		//  Not a valid time series interval		
		return STATUS_NOT_OKAY;
	}
	status = 0;

	//  Get the date from the pathname
	len = zpathnameGetPart (pathname, 4, dPart, sizeof(dPart));
	//  Get the Julian date
	if (len > 15) {
		//  Looks like a date range to specify time window
		//  e.g. "12Jun2002 - 04Oct2010"
		ptr = strstr(dPart, " - ");
		if (!ptr) {
			//  Not time series
			return STATUS_NOT_OKAY;
		}
		//  Get the first (starting) date
		ipos = (int)(ptr - dPart);
		stringCopy(date, sizeof(date), dPart, ipos);
		date[ipos] = '\0';
		startJulian = dateToJulian(date);
		if (startJulian == UNDEFINED_TIME) {
			return STATUS_NOT_OKAY;
		}
		//  Get the ending date.  Remove the " - "
		ipos += 3;
		len -= ipos;
		stringCopy(date, sizeof(date), &dPart[ipos], len);
		date[len] = '\0';
		endJulian = dateToJulian(date);
		if (endJulian == UNDEFINED_TIME) {
			return STATUS_NOT_OKAY;
		}
		status = 1;
		//  If the ending date is in caps, then this is the date of last pathname,
		//  not the ending date  (e.g.  "01JAN2001 - 01JAN2010").  The actual ending
		//  date is one block more, minus one day
		c = tolower(date[3]);
		if (c != date[3]) {
			boolAdjustEnd = 1;			
		}
		else {
			boolAdjustEnd = 0;
		}
		if (intervalSeconds > 0) {
			startBlockJulian = ztsRegGetBlockStart(startJulian, intervalSeconds, &blockSize);	
			if (boolAdjustEnd) {
				julian = ztsRegGetBlockStart(endJulian, intervalSeconds, &blockSize);
				if (julian == endJulian) {
					endJulian = ztsIncrementBlock(julian, blockSize) -1;
				}
			}
		}
		else {
			blockSize = -intervalSeconds;
			startBlockJulian = ztsIrregGetBlockStart(startJulian, blockSize);
			if (boolAdjustEnd) {
				julian = ztsIrregGetBlockStart(endJulian, blockSize);
				if (julian == endJulian) {
					endJulian = ztsIncrementBlock(julian, blockSize) -1;
				}
			}			
		}
		boolEndDateGiven = 1;
	}
	else if (len > 7) {
		//  Must be a single date for the D part, e.g., 04JUL1976
		startJulian = dateToJulian(dPart);
		if (startJulian == UNDEFINED_TIME) {
			return STATUS_NOT_OKAY;
		}
		//  Get the block size for this path
		if (intervalSeconds > 0) {
			startBlockJulian = ztsRegGetBlockStart(startJulian, intervalSeconds, &blockSize);
			//  Now the date of the start of the next block, which is the same as the end of our block
			endJulian = ztsIncrementBlock(startBlockJulian, blockSize);			
		}
		else {
			blockSize = -intervalSeconds;
			startBlockJulian = ztsIrregGetBlockStart(startJulian, blockSize);
			//  Now the date of the start of the next block, which is the same as the end of our block
			endJulian = ztsIncrementBlock(startBlockJulian, blockSize);
		}
		endJulian--;
		status = 1;
		boolEndDateGiven = 0;
	}
	else {
		//  Must not be a date...
		//  That's OK, just return with the times undefined...		
		return 0;
	}

	if (blockSize > 0) {
		timeWindow->blockSize = blockSize;
	}
	timeWindow->startBlockJulian = startBlockJulian;	
	timeWindow->startJulian = startJulian;	
	timeWindow->startTimeSeconds = startSecondsAfterMidnight;	
	timeWindow->endJulian = endJulian;	
	timeWindow->endTimeSeconds = SECS_IN_1_DAY;  //  End of day

	
	
	//  Calculate the number of values to read
	if (intervalSeconds > 0) {
		if ((timeWindow->numberValues <= 0) || (boolEndDateGiven)) {
			numberValues = numberPeriods(intervalSeconds, timeWindow->startJulian, intervalSeconds,
				timeWindow->endJulian, timeWindow->endTimeSeconds) + 1;
			timeWindow->numberValues = numberValues;
		}
		else {
			//  We already have the number of periods, so compute the end using that
			incrementTime(intervalSeconds, (timeWindow->numberValues - 1),
				timeWindow->startJulian, timeWindow->startTimeSeconds,
				&timeWindow->endJulian, &timeWindow->endTimeSeconds);
		}
	}

	return status;
}
