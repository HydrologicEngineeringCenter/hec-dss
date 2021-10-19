#include "heclib7.h"
#include "hecdssInternal.h"
/**
*  Function:	ztsIrregGetBlockStart
*
*  Use:			Private
*
*  Description:	Gets the Julian start date of a irregular interval time series block, based on the block size
*
*  Declaration: int ztsIrregGetBlockStart(int julianDate, int blockSize);
*
*  Parameters:	int julianDate
*					Julian date that is within the block returned
*
*				int blockSize
*					The block size (set by user / programmer), usually based on
*					amount of data or frequency
*
*
*	Returns:	int startBlockJulian
*					The julian date of the start of the block
*
*
*	Remarks:	blockSize:
*					1:  Daily block
*					2:  Monthly block
*					3:  Yearly block
*					4:  Decade block
*					5:  Century block
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/

int ztsIrregGetBlockStart(int julianDate, int blockSize)
{
	int startBlockJulian;
	int iyear;
	int imonth;
	int iday;

	julianToYearMonthDay(julianDate, &iyear, &imonth, &iday);

	//  Back up to the start of the block
	if (blockSize == 1) {
		//  Daily
	}
	else if (blockSize == 2) {
		//  Monthly, back up to the first day of the month
		iday = 1;
	}
	else if (blockSize == 3) {
		//  Yearly, back up to the first day of the year
		iday = 1;
		imonth = 1;
	}
	else if (blockSize == 4) {
		//  Decade, back up to the first day of the decade
		iday = 1;
		imonth = 1;
		iyear = (iyear/10) * 10;
	}
	else if (blockSize == 5) {
		//  Century, back up to the first day of the century
		iday = 1;
		imonth = 1;
		iyear = (iyear/100) * 100;
	}
	else {
		//  Error
		return STATUS_NOT_OKAY;
	}

	//  Now convert back to julian
	startBlockJulian = yearMonthDayToJulian(iyear, imonth, iday);
	return startBlockJulian;
}

