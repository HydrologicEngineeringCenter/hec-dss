#include <string.h>
#include <stdlib.h>

#include "heclib7.h"
#include "hecdssInternal.h"


/**
*  Function:	ztsInternalHeaderUnpack
*
*  Use:			Private
*
*  Description:	Copy units, type, time zone and precision from the internalHeader to a time series struct
*
*  Declaration: int ztsInternalHeaderUnpack(zStructTimeSeries *tss, int *internalHeader, int internalHeaderNumber);
*
*  Parameters:	zStructTimeSeries *tss
*					The time series struct to copy items too
*
*				int *internalHeader
*					The internal header, where items are copied from.
*
*				int internalHeaderNumber
*					The number of int 4 words used in the internal header array.
*
*
*
*	Returns:	int status
*					STATUS_OKAY
*					STATUS_NOT_OKAY:   Something bad happened, most likely exhausted memory
*
*	Remarks:	The character items are null terminated, even if they are not given (nulls)
*					They are given in the following order:  units, type, time zone
*					For profile it is:  depth units, value units, type, time zone
*
*	See Also:	ztsInternalHeaderPack()
*
*  Time series internal header:
*			INT_HEAD_precision				1
*           INT_HEAD_timeOffset				2
*           INT_HEAD_profileDepthsNumber	3
*           INT_HEAD_blockStartPosition		4
*           INT_HEAD_blockEndPosition		5
*           INT_HEAD_valuesNumber			6
*           INT_HEAD_valueSize				7
*           INT_HEAD_valueElementSize		8
*           INT_HEAD_valuesCompressionFlag	9
*           INT_HEAD_qualityNumber			10
*           INT_HEAD_qualityElementSize		11
*           INT_HEAD_qualityCompressionFlag 12
*           INT_HEAD_inotesNumber			13
*           INT_HEAD_inotesElementSize		14
*           INT_HEAD_inotesCompressionFlag	15
*           INT_HEAD_cnotesLength			16
*           INT_HEAD_units					17  
*****	Beginning of units have been moved to 18 for big endian machines (DSS Version 7-HA)
*			17 may either contain all blanks (for backwards compatibility) or 0.
*
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int ztsInternalHeaderUnpack(zStructTimeSeries *tss, int *internalHeader, int internalHeaderNumber)
{
	char carray[200];
	int ipos;
	int len;


	//  Safety check
	if (internalHeaderNumber < INT_HEAD_timeOffset) return STATUS_NOT_OKAY;

	if (internalHeader[INT_HEAD_timeGranularity] > 0) {
		tss->timeGranularitySeconds = internalHeader[INT_HEAD_timeGranularity];
	}
	tss->precision = internalHeader[INT_HEAD_precision];
	tss->timeOffsetSeconds = internalHeader[INT_HEAD_timeOffset];
	tss->timeWindow->timeOffsetSeconds = internalHeader[INT_HEAD_timeOffset];

	len = internalHeaderNumber - INT_HEAD_units;
	//  Do we have any units or type?  (which is okay)
	if (len <= 1) return STATUS_OKAY;


	//  The start of the units has been moved from int 17 to int 18 
	//  This is for updated files adopted for big endian machines and all strings
	//  are on 8-byte word boundaries
	//  Check to see if this record has not been updated yet (and units start in 17)
	//  If moved, int 17 will either be zero or all blanks

	if (!unitsHavePadding(internalHeader,INT_HEAD_units)) {
		//  Depreciated
		len *= 4;
		charInt(&internalHeader[INT_HEAD_units], carray, len, sizeof(carray), 0, 0, 0);		
	}
	else {
		len--;
		len *= 4;
		charLong(&internalHeader[INT_HEAD_units + 1], carray, len, sizeof(carray), 0, -1);
	}

	ipos = 0;

	len = (int)strlen(&carray[ipos]);
	if (len) {
		if (internalHeader[INT_HEAD_profileDepthsNumber] == 0) {
			if (trimLength(&carray[ipos])) {
				tss->units = mallocAndCopyTrim(&carray[ipos]);
				if (!tss->units) {
					return STATUS_NOT_OKAY;
				}
				tss->allocated[zSTRUCT_TS_units] = 1;
			}
			ipos += len + 1;
		}
		else {
			if (trimLength(&carray[ipos])) {
				tss->unitsProfileDepths = mallocAndCopyTrim(&carray[ipos]);
				if (!tss->unitsProfileDepths) {
					return STATUS_NOT_OKAY;
				}
				tss->allocated[zSTRUCT_TS_profileUnitsDepths] = 1;
			}
			ipos += len + 1;

			len = (int)strlen(&carray[ipos]);
			if (trimLength(&carray[ipos])) {
				tss->unitsProfileValues = mallocAndCopyTrim(&carray[ipos]);
				if (!tss->unitsProfileValues) {
					return STATUS_NOT_OKAY;
				}
				tss->allocated[zSTRUCT_TS_profileUnitsValues] = 1;
			}
			ipos += len + 1;
		}
	}

	len = (int)strlen(&carray[ipos]);
	if (len) {
		if (trimLength(&carray[ipos])) {
			tss->type = mallocAndCopyTrim(&carray[ipos]);
			if (!tss->type) {
				return STATUS_NOT_OKAY;
			}
			tss->allocated[zSTRUCT_TS_type] = 1;
		}
		ipos += len + 1;
	}

	len = (int)strlen(&carray[ipos]);
	if (len) {
		if (trimLength(&carray[ipos])) {
			tss->timeZoneName = mallocAndCopyTrim(&carray[ipos]);
			if (!tss->timeZoneName) {
				return STATUS_NOT_OKAY;
			}
			tss->allocated[zSTRUCT_timeZoneName] = 1;
		}
	}
	return STATUS_OKAY;
}

