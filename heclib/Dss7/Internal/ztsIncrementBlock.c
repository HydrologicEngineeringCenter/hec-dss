
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
	int multiplier;  //  -1 to decrement, +1 to increment
	int itime;
	int interval;
	int istime = 86400;
	int zero = 0;

	if (blockSize < 0) {
		multiplier = -1;
		blockSize = -blockSize;
	}
	else {
		multiplier = 1;
	}

	if (blockSize == DAILY_BLOCK) {
		//  Increment by one day
		julianNextBlockDate = julianBlockDate + multiplier;
	}
	else if (blockSize == MONTHLY_BLOCK) {
		//  Increment by one month
		interval = 43200*60;
		incrementTime(interval, multiplier, julianBlockDate, istime, &julianNextBlockDate, &itime);
	}
	else if (blockSize == YEARLY_BLOCK) {
		//  Increment by one year
		interval = 525600*60;
		incrementTime(interval, multiplier, julianBlockDate, istime, &julianNextBlockDate, &itime);
	}
	else if (blockSize == DECADE_BLOCK) {
		//  Increment by one decade
		multiplier *= 10;
		interval = 525600*60;
		incrementTime(interval, multiplier, julianBlockDate, istime, &julianNextBlockDate, &itime);
	}
	else if (blockSize == CENTURY_BLOCK) {
		//  Increment by one century
		multiplier *= 100;
		interval = 525600*60;
		incrementTime(interval, multiplier, julianBlockDate, istime, &julianNextBlockDate, &itime);
	}
	return julianNextBlockDate;
}

