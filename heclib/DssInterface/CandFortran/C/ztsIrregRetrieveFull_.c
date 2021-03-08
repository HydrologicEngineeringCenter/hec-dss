#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "zStructTsTimeWindow.h"



//  FORTRAN Interface

void ztsirregretrievefull_(long long *ifltab, const char *path,
							const char *startDate, const char *startTime,
							const char *endDate, const char *endTime,
							int *maxNumberValues, int *numberRead,
							int timeArray[], int *timeGranularitySeconds, int *julianBaseDate,
							int values[], int *sizeEachValueRequested, int *sizeEachValueRead,
							int quality[], int *qualityElementSizeRequested, int *qualityElementSizeRead,
							int notes[], int *inoteElementSizeRequested, int *sizeEachNoteRead,
							char *cnotesFort, int *totalSizeCNotes, int *totalNumberCnotesRead,
							int userHeader[], int *userHeaderArraySize, int *userHeaderNumber,
							char *units, char *type, int *precisionValues,             //  Note: no maxUnits or maxType
							char *timeZoneName, int *retrieveFlag, int *status,
							size_t pathLen, size_t startDateLen, size_t startTimeLen,
							size_t endDateLen, size_t endTimeLen, size_t cNotesLen,
							size_t unitsLen, size_t typeLen, size_t timeZoneNameLen)

{

	char *pathname;
	char *sDate;
	char *sTime;
	char *eDate;
	char *eTime;

	int boolGetQuality;
	int boolGetDoubles;
	int ipos;
	int jpos;
	int len;
	int lencnotesRead;
	int i;

	zStructTimeSeries *tss;


	pathname = stringFortToC(path, pathLen);
	sDate = stringFortToC(startDate, startDateLen);
	sTime = stringFortToC(startTime, startTimeLen);
	eDate = stringFortToC(endDate, endDateLen);
	eTime = stringFortToC(endTime, endTimeLen);


	if (*qualityElementSizeRequested > 0) {
		boolGetQuality = 1;
	}
	else {
		boolGetQuality = 0;
	}
	if (*sizeEachValueRequested > 1) {
		boolGetDoubles = 2;
	}
	else {
		boolGetDoubles = 1;
	}


	tss = zstructTsNewTimes(pathname, sDate, sTime, eDate, eTime);
	tss->numberValues = *maxNumberValues;
	*status = ztsRetrieve(ifltab, tss, 0, boolGetDoubles, boolGetQuality);

	if (*status >= 0) {
		*numberRead = tss->numberValues;
		for (i=0; i<tss->numberValues; i++) {
			timeArray[i] = tss->times[i];
		}
	// FIX ME - NEED to return julian base date
		*timeGranularitySeconds = tss->timeGranularitySeconds;
		*julianBaseDate = tss->julianBaseDate;
		if (boolGetDoubles == 2) {
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

		if (*qualityElementSizeRequested) {
			if (tss->qualityElementSize > 0) {
				//  Cannot handle anything other than single quality??
				*qualityElementSizeRead = tss->qualityElementSize;
				convertIntArray((void *)tss->quality, (void *)quality, tss->numberValues, tss->qualityElementSize, *qualityElementSizeRequested);
			}
			else {
				*qualityElementSizeRead = 0;
			}
		}
		else {
			*qualityElementSizeRead = 0;
		}

		if (*inoteElementSizeRequested) {
			if (tss->inoteElementSize > 0) {
				//  Cannot handle anything other than single quality??
				*sizeEachNoteRead = tss->inoteElementSize;
				convertIntArray((void *)tss->inotes, (void *)notes, tss->numberValues, tss->inoteElementSize, *inoteElementSizeRequested);
			}
			else {
				*sizeEachNoteRead = 0;
			}
		}
		else {
			*sizeEachNoteRead = 0;
		}

		stringCToFort(units, unitsLen,  tss->units);
		stringCToFort(type, typeLen,  tss->type);

		stringCToFort(timeZoneName, timeZoneNameLen,  tss->timeZoneName);

		if ((*totalSizeCNotes > 0) && (tss->cnotes != 0)){
			ipos = 0;
			jpos = 0;
			*totalNumberCnotesRead = tss->cnotesLengthTotal;
			lencnotesRead = *totalNumberCnotesRead;
			for (i=0; i<*numberRead; i++) {
				len = strnlen_hec(&tss->cnotes[jpos], (size_t)lencnotesRead);
				stringCToFort(&cnotesFort[ipos], cNotesLen,  &tss->cnotes[jpos]);
				jpos += len + 1;
				ipos += (int)cNotesLen;
				lencnotesRead -= len + 1;
				if (lencnotesRead <= 0) {
					break;
				}
			}
		}
		else {
			*totalNumberCnotesRead = 0;
		}

		if (*userHeaderArraySize > 0) {
			if (tss->userHeaderSize > 0) {
				len = tss->userHeaderSize;
				if (*userHeaderArraySize < len) len = *userHeaderArraySize;
				for (i=0; i<len; i++) {
					userHeader[i] = tss->userHeader[i];
				}
				*userHeaderNumber = len;
			}
		}
		*precisionValues = tss->precision;
	}

	free(pathname);
	free(sDate);
	free(sTime);
	free(eDate);
	free(eTime);
	zstructFree(tss);

}

