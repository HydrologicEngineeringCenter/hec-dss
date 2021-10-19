#include <stdlib.h>
#include <string.h>

#include "hecdss7.h"

/**
*  Function:	zstructTsNew
*
*  Use:			Public
*
*  Description:	Creates a new time series struct
*
*  Declaration: zStructTimeSeries* zstructTsNew(const char* pathname);
*
*  Parameters:	const char* pathname
*					The pathname for this struct.  Must be a valid pathname.
*					A copy of the pathname is used in struct.
*
*
*
*	Returns:	zStructTimeSeries*
*					An address to the time series struct created.
*
*   Note:		ALWAYS call function zstructFree(struct) when finished using.
*
*	Remarks:	This is the standard struct used by the various DSS time series functions.
*
*	See:		ztsStore and ztsRetrieve for definition of zStructTimeSeries
*
*	See Also:	zstructTsNewTimes()
*				zstructTsNewRegFloats()
*				zstructTsNewRegDoubles()
*				zstructTsNewIrregFloats()
*				zstructTsNewIrregDoubles()
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


zStructTimeSeries* zstructTsNew(const char* pathname)
{
	zStructTimeSeries *tss;
	int i;
	int len;

	len = sizeof(zStructTimeSeries);
	tss = (zStructTimeSeries*)calloc((size_t)len, BYTE_SIZE);
	if (!tss) {
		return tss;
	}

	for (i=0; i<zSTRUCT_length; i++) {
		tss->allocated[i] = 0;
	}

	if (pathname) {
		tss->pathname = mallocAndCopyPath(pathname);
		tss->allocated[zSTRUCT_pathname] = 1;
	}

	tss->startJulianDate = UNDEFINED_TIME;
	tss->startTimeSeconds = -1;
	tss->endJulianDate = UNDEFINED_TIME;
	tss->endTimeSeconds = -1;
	tss->precision = -1;

	//  Read any user header by default
	tss->userHeaderSize = 1;  //  1 is a flag to allocate memory at read time

	//  Time series type (all time series)
	tss->structType = DATA_TYPE_RTS;

	tss->locationStruct = zstructLocationNew(pathname);
	tss->allocated[zSTRUCT_TS_locationStruct] = 1;

	return tss;
}

