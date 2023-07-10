
#include "hecdssInternal.h"


/**
*  Function:	ztsIncrementBlock
*
*  Use:			Private
*
*  Description:	Increments or decrements (by one) the Julian date of a time series block
*
*  Declaration: int ztsIncrementBlock(int julianBlockDate, int blockSize);
*
*  Parameters:	int julianBlockDate
*					The julian date of the current block to increment from.
*
*				int blockSize
*					A flag that tells the size of the block.  To decrement by one, set this to negative
*
*
*
*	Returns:	int julianNextBlockDate
*					The Julian date of the next (or previous) block in sequence
*
*	Remarks:	blockSize:
*					1:  Daily block
*					2:  Monthly block
*					3:  Yearly block
*					4:  Decade block
*					5:  Century block
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int ztsIncrementBlock(int julianBlockDate, int blockSize)
{
	int julianNextBlockDate = 0;
	int step;  //  -1 to decrement, +1 to increment

	if (blockSize < 0) {
		step = -1;
		blockSize = -blockSize;
	}
	else {
		step = 1;
	}
	if (blockSize == DAILY_BLOCK) {
		//  Increment by one day
		julianNextBlockDate = julianBlockDate + step;
	}
	else {
		int yr, mon, day;
		julianToYearMonthDay(julianBlockDate, &yr, &mon, &day);
		switch (blockSize) {
		case MONTHLY_BLOCK:
			//  Increment by one month
			mon += step;
			switch (mon) {
			case 0:
				mon = 12;
				yr -= 1;
				break;
			case 13:
				mon = 1;
				yr += 1;
				break;
			}
			break;
		case YEARLY_BLOCK:
			//  Increment by one year
			yr += step;
			break;
		case DECADE_BLOCK:
			//  Increment by one decade
			yr += 10 * step;
			break;
		case CENTURY_BLOCK:
			//  Increment by one century
			yr += 100 * step;
		}
		julianNextBlockDate = yearMonthDayToJulian(yr, mon, day);
	}
	return julianNextBlockDate;
}

