
#include "hecdssInternal.h"
#include "standardIntervals.h"


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
	int itime;
	int istime = SECS_IN_1_DAY;
	int zero = 0;

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
	else if (blockSize == MONTHLY_BLOCK) {
		//  Increment by one month
		incrementTime(SECS_IN_1_MONTH, step, julianBlockDate, istime, &julianNextBlockDate, &itime);
	}
	else if (blockSize == YEARLY_BLOCK) {
		//  Increment by one year
		incrementTime(SECS_IN_1_YEAR, step, julianBlockDate, istime, &julianNextBlockDate, &itime);
	}
	else if (blockSize == DECADE_BLOCK) {
		//  Increment by one decade
		incrementTime(SECS_IN_1_YEAR, step * 10, julianBlockDate, istime, &julianNextBlockDate, &itime);
	}
	else if (blockSize == CENTURY_BLOCK) {
		//  Increment by one century
		incrementTime(SECS_IN_1_YEAR, step * 100, julianBlockDate, istime, &julianNextBlockDate, &itime);
	}
	return julianNextBlockDate;
}

