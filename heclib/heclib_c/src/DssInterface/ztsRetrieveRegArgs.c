#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "zStructTsTimeWindow.h"



//  Full method to retrieve regular interval time series data,
//  passing arguments instead of a struct
//  Used for Fortran compatibility

int  ztsRetrieveRegArgs(long long *ifltab, const char *pathname,
						const char *startDate, const char *startTime, int *timeOffsetSeconds,
						int maxNumberValues, int *numberRead,
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


	tss = zstructTsNewTimes(pathname, startDate, startTime, "", "");
	tss->numberValues = maxNumberValues;

	if ((qualityElementSizeRequested > 0) || (inoteElementSizeRequested > 0) || (totalSizeCNotes > 0)) {
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

	status = ztsRetrieve(ifltab, tss, -2, retrieveDoublesFlag, boolGetQuality);

	if (zisError(status)) {
		zstructFree(tss);
	}

	if (status == STATUS_RECORD_NOT_FOUND) {
		zstructFree(tss);
	}


	*numberRead = tss->numberValues;

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
	if (tss->timeZoneName) {
		stringCopy(timeZoneName, sizeOfTimeZoneName, tss->timeZoneName, strlen(tss->timeZoneName));
	}

	if ((totalSizeCNotes > 0) && (tss->cnotesLengthTotal > 0)){
		for (i=0; i<tss->cnotesLengthTotal; i++) {
			cnotes[i] = tss->cnotes[i];
		}
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

	*timeOffsetSeconds = tss->timeOffsetSeconds;
	*precisionValues = tss->precision;

	zstructFree(tss);

	return status;

}

//  FORTRAN Interface


void ztsretrieveregargs_(long long *ifltab, const char *path,
				const char *startDate, const char *startTime, int *timeOffsetSeconds,
				int *maxNumberValues, int *numberRead,
				int values[], int *sizeEachValueRequested, int *sizeEachValueRead,
				int quality[], int *qualityElementSizeRequested, int *qualityElementSizeRead,
				int notes[], int *inoteElementSizeRequested, int *sizeEachNoteRead,
				char *cnotesFort, int *totalSizeCNotes, int *totalNumberCnotesRead,
				int userHeader[], int *userHeaderArraySize, int *userHeaderNumber,
				char *units, char *type,
				int *precisionValues, char *timeZoneName, int *status,
				size_t pathLen, size_t startDateLen, size_t startTimeLen, size_t cNotesLen,
				size_t unitsLen, size_t typeLen, size_t timeZoneLen)


{

	char *pathname;
	char *sDate;
	char *sTime;
	char cunits[13];
	char ctype[13];
	char ctimeZoneName[30];
	char *cnotes;
	int ipos;
	int jpos;
	int len;
	int lencnotesRead;
	int i;

	pathname = stringFortToC(path, pathLen);
	sDate = stringFortToC(startDate, startDateLen);
	sTime = stringFortToC(startTime, startTimeLen);

	if (*totalSizeCNotes > 0) {
		cnotes = (char *)malloc((size_t)*totalSizeCNotes);
	}
	else {
		cnotes = 0;
	}


	*status = ztsRetrieveRegArgs(ifltab, pathname,
					sDate, sTime, timeOffsetSeconds,
					*maxNumberValues, numberRead,
					values, *sizeEachValueRequested, sizeEachValueRead,
					quality, *qualityElementSizeRequested, qualityElementSizeRead,
					notes, *inoteElementSizeRequested, sizeEachNoteRead,
					cnotes, *totalSizeCNotes, totalNumberCnotesRead,
					userHeader, *userHeaderArraySize, userHeaderNumber,
					cunits, sizeof(cunits), ctype, sizeof(ctype), precisionValues,
					ctimeZoneName, sizeof(ctimeZoneName));

	if (*status >= 0) {
		stringCToFort(units, unitsLen,  cunits);
		stringCToFort(type, typeLen,  ctype);
		stringCToFort(timeZoneName, timeZoneLen,  ctimeZoneName);
		if ((*totalNumberCnotesRead > 0) && (cnotes != 0)){
			ipos = 0;
			jpos = 0;
			lencnotesRead = *totalNumberCnotesRead;
			for (i=0; i<*numberRead; i++) {
				len = strnlen_hec(&cnotes[jpos], (size_t)lencnotesRead);
				stringCToFort(&cnotesFort[ipos], cNotesLen,  &cnotes[jpos]);
				jpos += len + 1;
				ipos += cNotesLen;
				lencnotesRead -= len + 1;
				if (lencnotesRead <= 0) {
					break;
				}
			}
		}
	}
	if (cnotes) {
		free(cnotes);
		cnotes = 0;
	}
	free(pathname);
	free(sDate);
	free(sTime);
}

