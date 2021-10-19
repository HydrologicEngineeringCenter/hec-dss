#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "heclib.h"
#include "zStructTsTimeWindow.h"
#include "zStructTimeSeries.h"
#include "zerrorCodes.h"


/**
*  Function:	ztsTrim
*
*  Use:			Private
*
*  Description:	Trims missing data from beginning and end of arrays in a zStructTimeSeries
*
*  Declaration: int ztsTrim(long long *ifltab, zStructTimeSeries *tss);
*
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				zStructTimeSeries *tss
*					The time series struct.
*
*
*	Returns:	int status
*					STATUS_OKAY
*					STATUS_NOT_OKAY - No valid data was found (all missing or no data array)
*					error code - value contains a description of the error and where it occurred.
*					Probably memory exhaustion.  See zerrorDecode for descriptions.
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

///   FIX ME - USE ztsFirstLastValid

int ztsTrim(long long *ifltab, zStructTimeSeries *tss)
{
	int numberValues;
	int firstValid;
	int lastValid;
	int count;
	int i;
	int j;
	int n;
	int end;
	int endCount;
	int numberToMalloc;
	float *floatValues;
	double *doubleValues;
	int *times;
	int *quality;
	int *inotes;
	char *cnotes;
	char cdummy[1];


	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztsTrim_ID, "Enter, Pathname: ",tss->pathname);
	}


	if (tss->numberValues <= 0) {
		return STATUS_NOT_OKAY;
	}

	firstValid = -1;
	lastValid = tss->numberValues;  //  (has to be > than last value position)
	numberValues = tss->numberValues;

	//////////////////////  FIX ME - CNOTES THAT ARE NOT \0 SHOULD NOT
	///////////////  BE CONSIDERED MISSING!!!!!!!!!!

	if (tss->profileDepthsNumber == 0) {
		//  Not profile data, usual case...
		if (tss->floatValues) {
			for (i=0; i<numberValues; i++) {
				if (!zisMissingFloat(tss->floatValues[i])) {
					firstValid = i;
					break;
				}
			}
			if (firstValid == -1) {
				numberValues = 0;
			}
			else {
				for (i=(numberValues-1); i>=0; i--) {
					if (!zisMissingFloat(tss->floatValues[i])) {
						lastValid = i;
						break;
					}
				}
			}
		}
		else if (tss->doubleValues) {
			for (i=0; i<numberValues; i++) {
				if (!zisMissingDouble(tss->doubleValues[i])) {
					firstValid = i;
					break;
				}
			}
			if (firstValid == -1) {
				numberValues = 0;
			}
			else {
				for (i=(numberValues-1); i>=0; i--) {
					if (!zisMissingDouble(tss->doubleValues[i])) {
						lastValid = i;
						break;
					}
				}
			}
		}
		else {
			// No array?  (Shouldn't happen, and if does, is caught elsewhere)
			return STATUS_NOT_OKAY;
		}

		if ((firstValid > 0)  || (lastValid < (numberValues -1))) {
			//  Need to trim data sets
			numberValues = lastValid - firstValid + 1;
			if (tss->floatValues) {
				floatValues = (float *)malloc((size_t)numberValues * 4);
				if (floatValues) {
					count = 0;
					for (i=firstValid; i<(firstValid + numberValues); i++) {
						floatValues[count++] = tss->floatValues[i];
					}
					free(tss->floatValues);
					tss->floatValues = floatValues;
				}
				else {
					n = numberValues * 4;
					return zerrorProcessing(ifltab, DSS_FUNCTION_ztsTrim_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, n, 0,
											zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating floatValues");
				}
			}
			else {
				doubleValues = (double *)malloc((size_t)numberValues * 8);
				if (doubleValues) {
					count = 0;
					for (i=firstValid; i<(firstValid + numberValues); i++) {
						doubleValues[count++] = tss->doubleValues[i];
					}
					free(tss->doubleValues);
					tss->doubleValues = doubleValues;
				}
				else {
					n = numberValues * 8;
					return zerrorProcessing(ifltab, DSS_FUNCTION_ztsTrim_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, n, 0,
											zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating doubleValues");
				}
			}
		}
		else if (firstValid == -1) {
			//  All missing!
			tss->numberValues = 0;
			return STATUS_NOT_OKAY;
		}
	}
	else {

		//  Profile data
		if (tss->floatProfileValues) {
			count = 0;
			for (i=0; i<numberValues; i++) {
				for (j=0; j<tss->profileDepthsNumber; j++) {
					//  tss->floatProfileValues[j][i]
					if (!zisMissingFloat(tss->floatProfileValues[count++])) {
						firstValid = i;
						break;
					}
				}
				if (firstValid != -1)
					break;
			}
			if (firstValid == -1) {
				numberValues = 0;
			}
			else {
				for (i=(numberValues-1); i>=0; i--) {
					for (j=0; j<tss->profileDepthsNumber; j++) {
						count = (i * tss->profileDepthsNumber) + j;
						//  tss->floatProfileValues[j][i]
						if (!zisMissingFloat(tss->floatProfileValues[count])) {
							lastValid = i;
							break;
						}
					}
					if (lastValid == i)
						break;
				}
			}
		}
		else if (tss->doubleProfileValues) {
			count = 0;
			for (i=0; i<numberValues; i++) {
				for (j=0; j<tss->profileDepthsNumber; j++) {
					//  tss->doubleProfileValues[j][i]
					if (!zisMissingDouble(tss->doubleProfileValues[count++])) {
						firstValid = i;
						break;
					}
				}
				if (firstValid != -1)
					break;
			}
			if (firstValid == -1) {
				numberValues = 0;
			}
			else {
				for (i=(numberValues-1); i>=0; i--) {
					for (j=0; j<tss->profileDepthsNumber; j++) {
						count = (i * tss->profileDepthsNumber) + j;
						//  tss->doubleProfileValues[j][i]
						if (!zisMissingDouble(tss->doubleProfileValues[count])) {
							lastValid = i;
							break;
						}
					}
					if (lastValid == i)
						break;
				}
			}
		}
		else {
			// fix
			return STATUS_NOT_OKAY;
		}

		if ((firstValid > 0) || (lastValid != (numberValues -1))) {
			//  Need to trim data sets
			numberValues = lastValid - firstValid + 1;
			numberToMalloc = numberValues * tss->profileDepthsNumber;
			if (tss->floatProfileValues) {
				floatValues = (float *)malloc((size_t)numberToMalloc * 4);
				if (!floatValues) {
					n = numberToMalloc * 4;
					return zerrorProcessing(ifltab, DSS_FUNCTION_ztsTrim_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, n, 0,
											zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating floatProfileValues");
				}
				count = 0;
				for (i=firstValid; i<(firstValid + numberValues); i++) {
					for (j=0; j<tss->profileDepthsNumber; j++) {
						n = (i * tss->profileDepthsNumber) + j;
						floatValues[count++] = tss->floatProfileValues[n];
						//  floatValues[j][count++] = tss->floatProfileValues[j][i];
					}
				}
				free(tss->floatProfileValues);
				tss->floatProfileValues = floatValues;
			}
			else {
				doubleValues = (double *)malloc((size_t)numberToMalloc * 8);
				if (!doubleValues) {
					n = numberToMalloc * 8;
					return zerrorProcessing(ifltab, DSS_FUNCTION_ztsTrim_ID,
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, n, 0,
											zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating doubleValues");
				}
				count = 0;
				for (i=firstValid; i<(firstValid + numberValues); i++) {
					for (j=0; j<tss->profileDepthsNumber; j++) {
						n = (i * tss->profileDepthsNumber) + j;
						doubleValues[count++] = tss->doubleProfileValues[n];
						//  doubleValues[j][count++] = tss->doubleProfileValues[j][i];
					}
				}
				free(tss->doubleProfileValues);
				tss->doubleProfileValues = doubleValues;
			}
		}
		else if (firstValid == -1) {
			//  All missing!
			tss->numberValues = 0;
			return STATUS_NOT_OKAY;
		}
	}

	//  Now adjust times, quality and notes
	if ((firstValid > 0)  || (lastValid < (tss->numberValues -1))) {
		ztsTrimAdjustTimeWindow(ifltab, tss, firstValid, lastValid);
		if (tss->times) {
			times = (int *)malloc((size_t)numberValues * 4);
			if (!times) {
				n = numberValues * 4;
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsTrim_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, n, 0,
										zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating times");
			}
			count = 0;
			for (i=firstValid; i<(firstValid + numberValues); i++) {
				times[count++] = tss->times[i];
			}
			free(tss->times);
			tss->times = times;
		}
		if (tss->quality) {
			quality = (int *)malloc((size_t)numberValues * tss->qualityElementSize * 4);
			if (!quality) {
				n = numberValues * tss->qualityElementSize * 4;
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsTrim_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, n, 0,
										zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating quality");
			}
			count = 0;
			for (i=firstValid; i<(firstValid + numberValues); i++) {
				for (j=0; j<tss->qualityElementSize; j++) {
						n = (i * tss->qualityElementSize) + j;
						quality[count++] = tss->quality[n];
						//  quality[j][count++] = tss->quality[j][i];
					}
			}
			free(tss->quality);
			tss->quality = quality;
		}
		if (tss->inotes) {
			inotes = (int *)malloc((size_t)numberValues * tss->inoteElementSize * 4);
			if (!inotes) {
				n = numberValues * tss->inoteElementSize * 4;
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsTrim_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, n, 0,
										zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating inotes");
			}
			count = 0;
			for (i=firstValid; i<(firstValid + numberValues); i++) {
				for (j=0; j<tss->inoteElementSize; j++) {
						n = (i * tss->inoteElementSize) + j;
						inotes[count++] = tss->inotes[n];
					}
			}
			free(tss->inotes);
			tss->inotes = inotes;
		}
		if (tss->cnotes) {
			//  Count the number of characters we'll need
			count = 0;
			if (firstValid > 0) {
				//  Count through the first startArray lines
				cdummy[0] = '\0';
				count = copyLines(cdummy, (size_t)0, tss->cnotes, (size_t)tss->cnotesLengthTotal, firstValid);
			}
			end = firstValid + numberValues -1;
			endCount = tss->cnotesLengthTotal;
			if (lastValid < end) {
				//  Find the position for the last spot
				cdummy[0] = '\0';
				endCount = copyLines(cdummy, (size_t)0, tss->cnotes, (size_t)tss->cnotesLengthTotal, lastValid);
			}
			numberToMalloc = endCount - count;
			cnotes = (char*)calloc((size_t)(numberToMalloc+1), 1);
			if (!cnotes) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsTrim_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, numberToMalloc, 0,
										zdssErrorSeverity.MEMORY_ERROR, tss->pathname, "Allocating cnotes");
			}
			tss->cnotesLengthTotal = copyLines(cnotes, (size_t)numberToMalloc, &tss->cnotes[count], (size_t)numberToMalloc, numberValues);
			free(tss->cnotes);
			tss->cnotes = cnotes;
		}
	}

	return 0;
}

