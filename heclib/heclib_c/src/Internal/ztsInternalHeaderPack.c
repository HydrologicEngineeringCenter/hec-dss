#include <string.h>

#include "heclib7.h"
#include "hecdssInternal.h"


/**
*  Function:	ztsInternalHeaderPack
*
*  Use:			Private
*
*  Description:	Copies a time series dataset unit, type, time zone and precision into the internalHeader for storing
*
*  Declaration: int ztsInternalHeaderPack(zStructTimeSeries *tss, int *internalHeader);
*
*  Parameters:	zStructTimeSeries *tss
*					The time series struct to copy items from
*
*				int *internalHeader
*					The internal header, where items are copied too.
*				
*
*	Returns:	int numberInts 
*					The number of ints (used) in the header (but on 8-byte boundary,
*					so this number will always be even.)
*	
*	Remarks:	The character items are null terminated, even if they are not given (nulls)
*					They are copied in the following order:  units, type, time zone
*					For profile it is:  depth units, value units, type, time zone
*
*	NOTE:		For big / little endian compatibility, ALL character strings have
*					to end on 8-byte (word) boundaries.  This is accomplished by
*					padding the end with blanks.  For example, "cfs" becomes
*					"cfs____\0" (where _ means a blank character)
*
*  Time series internal header:
#			INT_HEAD_timeGranularity		0
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
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int ztsInternalHeaderPack(zStructTimeSeries *tss, int *internalHeader)
{
	char carray[300];
	int ipos;
	int numberInts;
	int carraySize;


	//  Combine the character items into a single char array
	carraySize = sizeof(carray) - 2;   // Make a little less to be safe
	ipos = 0;

	if (tss->profileDepthsNumber == 0) {
		appendStringToHeader(tss->units, carray,&ipos, carraySize);
	}
	else {
		//  Profile 
		appendStringToHeader(tss->unitsProfileDepths, carray, &ipos, carraySize);
		appendStringToHeader(tss->unitsProfileValues, carray, &ipos, carraySize);
	}

	if (ipos >= carraySize) return numberIntsInBytes(carraySize);

	appendStringToHeader(tss->type, carray, &ipos, carraySize);
	
	if (ipos >= carraySize) return numberIntsInBytes(carraySize);

	appendStringToHeader(tss->timeZoneName, carray, &ipos, carraySize);
	
	if (ipos >= carraySize) return numberIntsInBytes(carraySize);

	//  Blank fill int word 17 (INT_HEAD_units) for compatibility with big endian machines
	charInt("    ", &internalHeader[INT_HEAD_units], 4, 4, 1, 0, isOdd(INT_HEAD_units));
	charLong(carray, &internalHeader[INT_HEAD_units + 1], ipos, ipos, 1, 1);
	// Copy the char array into the header
	//charInt (carray, &internalHeader[INT_HEAD_units], ipos, ipos, 1, 1, isOdd(INT_HEAD_units));

	//  ipos should be divisble by 8 and numberInts should be even to work with 64-bit words / addressing
	numberInts = numberIntsInBytes(ipos) + INT_HEAD_units + 1;

	return numberInts;
}
