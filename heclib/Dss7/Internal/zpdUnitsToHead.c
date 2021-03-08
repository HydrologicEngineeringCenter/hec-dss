#include <string.h>
#include <stdlib.h>

#include "heclib7.h"
#include "hecdssInternal.h"

/**
*  Function:	zerrorDecode
*
*  Use:			Private (internal)
*
*  Description:	Copy parameters from the paired data struct into the internal header
*				Copy parameters from the internal header into the paired data struct
*
*  Declaration: int zpdUnitsToHead(zStructPairedData *pds, int *internalHeader, int internalHeaderArraySize);
*				int zpdHeadToUnits(zStructPairedData *pds, int *internalHeader, int internalHeaderNumber);
*
*  Parameters:	zStructPairedData *pds
*					The paired data struct.
*
*				int *internalHeader
*					The internal header.
*
*				int internalHeaderArraySize
*					The size of the internal header array.
*
*				int internalHeaderNumber
*					The int length of the internal header array.
*

*
*	Returns:	int intLength
*					The space used in the internal header array, in ints
*					if negative, then an error occurred
*
*
*	NOTE:		For big / little endian compatibility, ALL character strings have
*					to end on 8-byte (word) boundaries.  This is accomplished by
*					padding the end with blanks.  For example, "cfs" becomes
*					"cfs____\0" (where _ means a blank character)
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zpdUnitsToHead(zStructPairedData *pds, int *internalHeader, int internalHeaderArraySize )
{
	char carray[400];
	int ipos;
	int len;
	char blankString[8];
	int carraySize;
	int internalHeaderCharSize;

	stringFill(blankString, ' ', sizeof(blankString) - 1);
	blankString[7] = '\0';
	carraySize = sizeof(carray) - 2;   // Make a little less to be safe
	ipos = 0;
	appendStringToHeader(pds->unitsIndependent, carray,&ipos,carraySize);
	if (ipos >= carraySize) return numberIntsInBytes(carraySize);
	
	appendStringToHeader(pds->typeIndependent, carray,&ipos,carraySize);
	if (ipos >= carraySize) return numberIntsInBytes(carraySize);

	appendStringToHeader(pds->unitsDependent, carray, &ipos, carraySize);
	if (ipos >= carraySize) return numberIntsInBytes(carraySize);

	appendStringToHeader(pds->typeDependent, carray, &ipos, carraySize);
	if (ipos >= carraySize) return numberIntsInBytes(carraySize);

	appendStringToHeader(pds->timeZoneName, carray, &ipos, carraySize);
	if (ipos >= carraySize) return numberIntsInBytes(carraySize);


	//  Make sure we don't overrun the int array
	internalHeaderCharSize = (internalHeaderArraySize - INT_HEAD_pdUnits) * 4;
	if (ipos > internalHeaderCharSize) ipos = internalHeaderCharSize;

	//  Blank fill int word 5 (INT_HEAD_pdUnits) for compatibility with big endian machines
	charInt("    ", &internalHeader[INT_HEAD_pdUnits], 4, 4, 1, 0, isOdd(INT_HEAD_pdUnits));
	charLong(carray, &internalHeader[INT_HEAD_pdUnits + 1], ipos, ipos, 1, 1);

	//  ipos should be divisble by 8 and numberInts should be even to work with 64-bit words / addressing
	len = numberIntsInBytes(ipos) + INT_HEAD_pdUnits + 1;
	return len;
}


int zpdHeadToUnits(zStructPairedData *pds, int *internalHeader, int internalHeaderNumber)
{
	char carray[400];
	int ipos;
	int len;
	

	len = internalHeaderNumber - INT_HEAD_pdUnits;
	//  Do we have any units or type?  (which is okay)
	if (len <= 0) return len;

	//  The start of the units has been moved from int 5 to int 6 
	//  This is for updated files adopted for big endian machines and all strings
	//  are on 8-byte word boundaries
	//  Check to see if this record has not been updated yet

	if (!unitsHavePadding(internalHeader, INT_HEAD_pdUnits)) {
		//  Depreciated
		len *= 4;
		charInt(&internalHeader[INT_HEAD_pdUnits], carray, len, sizeof(carray), 0, 0, 1);
	}
	else {
		len--;
		len *= 4;
		charLong(&internalHeader[INT_HEAD_pdUnits + 1], carray, len, sizeof(carray), 0, -1);
	}


	ipos = 0;
	len = (int)strlen(&carray[ipos]);
	if (!len) return 0;
	if (trimLength(&carray[ipos])) {
		pds->unitsIndependent = mallocAndCopyTrim(&carray[ipos]);
		if (!pds->unitsIndependent) {
			return STATUS_NOT_OKAY;
		}
		pds->allocated[zSTRUCT_unitsIndependent] = 1;
	}
	ipos += len + 1;

	len = (int)strlen(&carray[ipos]);
	if (!len) return 0;
	if (trimLength(&carray[ipos])) {
		pds->typeIndependent = mallocAndCopyTrim(&carray[ipos]);
		if (!pds->typeIndependent) {
			return STATUS_NOT_OKAY;
		}
		pds->allocated[zSTRUCT_typeIndependent] = 1;
	}
	ipos += len + 1;

	len = (int)strlen(&carray[ipos]);
	if (!len) return 0;
	if (trimLength(&carray[ipos])) {
		pds->unitsDependent = mallocAndCopyTrim(&carray[ipos]);
		if (!pds->unitsDependent) {
			return STATUS_NOT_OKAY;
		}
		pds->allocated[zSTRUCT_unitsDependent] = 1;
	}
	ipos += len + 1;

	len = (int)strlen(&carray[ipos]);
	if (!len) return 0;
	if (trimLength(&carray[ipos])) {
		pds->typeDependent = mallocAndCopyTrim(&carray[ipos]);
		if (!pds->typeDependent) {
			return STATUS_NOT_OKAY;
		}
		pds->allocated[zSTRUCT_typeDependent] = 1;
	}
	ipos += len + 1;

	len = (int)strlen(&carray[ipos]);
	if (!len) return 0;
	if (trimLength(&carray[ipos])) {
		pds->timeZoneName = mallocAndCopyTrim(&carray[ipos]);
		if (!pds->timeZoneName) {
			return STATUS_NOT_OKAY;
		}
		pds->allocated[zSTRUCT_timeZoneName] = 1;
	}
	return STATUS_OKAY;
}
