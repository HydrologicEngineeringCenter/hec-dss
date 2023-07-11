#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "zStructTsTimeWindow.h"


//  Full method to retrieve irregular interval time series data,
//  passing arguments instead of a struct
//  Used for Fortran compatibility

int  ztsRetrieveIrregArgs(long long *ifltab, const char *pathname,
						const char *startDate, const char *startTime,
						const char *endDate, const char *endTime,
						int maxNumberValues, int *numberRead, int itimes[],
						int values[], int sizeEachValueRequested, int *sizeEachValueRead,
						int quality[], int qualityElementSizeRequested, int *qualityElementSizeRead,
						int notes[], int inoteElementSizeRequested, int *sizeEachNoteRead,
						char *cnotes, int totalSizeCNotes, int *totalNumberCnotesRead,
						int userHeader[], int userHeaderArraySize, int *userHeaderNumber,
						char *units, int sizeOfUnits, char *type, int sizeOfType,
						int *precisionValues, char *timeZoneName, int sizeOfTimeZoneName)


{
	int status;
	int boolGetQuality;
	int retrieveDoublesFlag;
	int len;
	int i;

	zStructTimeSeries *tss;


	tss = zstructTsNewTimes(pathname, startDate, startTime, endDate, endTime);
	tss->numberValues = maxNumberValues;

	if (qualityElementSizeRequested > 0) {
		boolGetQuality = 1;
	}
	else {
		boolGetQuality = 0;
	}
	if (sizeEachValueRequested > 1) {
		retrieveDoublesFlag = 2;
	}
	else {
		retrieveDoublesFlag = 1;
	}

	status = ztsRetrieve(ifltab, tss, 0, retrieveDoublesFlag, boolGetQuality);

	if (zisError(status)) {
		zstructFree(tss);
	}

	if (status == STATUS_RECORD_NOT_FOUND) {
		zstructFree(tss);
	}


	*numberRead = tss->numberValues;

	convertIntArray(tss->times, itimes, tss->numberValues, 1, 1);
	if (tss->timeGranularitySeconds == SECOND_GRANULARITY) {
		for (i=0; i<tss->numberValues; i++) {
			itimes[i] /= SECS_IN_1_MINUTE;
		}
	}


	if (retrieveDoublesFlag == 2) {
		if (tss->doubleValues) {
			convertDataArray((void *)tss->doubleValues, (void *)values, tss->numberValues, 2, 2);
		}
		else if (tss->floatValues) {
			convertDataArray((void *)tss->floatValues, (void *)values, tss->numberValues, 1, 2);
		}
		else {
			//  Nasty error here
		}
	}
	else {
		if (tss->doubleValues) {
			convertDataArray((void *)tss->doubleValues, (void *)values, tss->numberValues, 2, 1);
		}
		else if (tss->floatValues) {
			convertDataArray((void *)tss->floatValues, (void *)values, tss->numberValues, 1, 1);
		}
		else {
			//  Nasty error here
		}
	}
	*sizeEachValueRead = tss->sizeEachValueRead;

	if (qualityElementSizeRequested) {
		if (tss->qualityElementSize > 0) {
			//  Cannot handle anything other than single quality??
			*qualityElementSizeRead = tss->qualityElementSize;
			convertIntArray((void *)tss->quality, (void *)quality, tss->numberValues, tss->qualityElementSize, qualityElementSizeRequested);
		}
		else {
			*qualityElementSizeRead = 0;
		}
	}
	else {
		*qualityElementSizeRead = 0;
	}

	if (inoteElementSizeRequested) {
		if (tss->inoteElementSize > 0) {
			//  Cannot handle anything other than single size?
			*sizeEachNoteRead = tss->inoteElementSize;
			convertIntArray((void *)tss->inotes, (void *)notes, tss->numberValues, tss->inoteElementSize, inoteElementSizeRequested);
		}
		else {
			*sizeEachNoteRead = 0;
		}
	}
	else {
		*sizeEachNoteRead = 0;
	}

	stringCopy(units, sizeOfUnits, tss->units, strlen(tss->units));
	stringCopy(type, sizeOfType, tss->type, strlen(tss->type));
	stringCopy(timeZoneName, sizeOfTimeZoneName, tss->timeZoneName, strlen(tss->timeZoneName));

	if ((totalSizeCNotes > 0) && (tss->cnotesLengthTotal > 0)){
		stringCopy(cnotes, totalSizeCNotes, tss->cnotes, tss->cnotesLengthTotal);
		*totalNumberCnotesRead = tss->cnotesLengthTotal;
	}
	else {
		*totalNumberCnotesRead = 0;
	}

	if (userHeaderArraySize > 0) {
		if (tss->userHeaderSize > 0) {
			len = tss->userHeaderSize;
			if (userHeaderArraySize < len) len = userHeaderArraySize;
			for (i=0; i<len; i++) {
				userHeader[i] = tss->userHeader[i];
			}
			*userHeaderNumber = len;
		}
	}

	*precisionValues = tss->precision;

	zstructFree(tss);

	return status;

}

