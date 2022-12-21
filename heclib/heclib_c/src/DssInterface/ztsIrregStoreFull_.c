#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "zStructTsTimeWindow.h"




//  timeArrayUnits:  The number of seconds a unit in *timeArray represents, usually
//					MINUTE_GRANULARITY (60) or SECOND_GRANULARITY (1)
//  storageFlag:	0:  Merge data sets together
//					1:  Replace data set (preferred)

void ztsirregstorefull_(long long *ifltab, const char *path,
						const char *baseDate, int *timeArrayUnits,
						int timeArray[], int *numberValues,
						int values[], int *valueSize,
						int quality[], int *qualityElementSize,
						int notes[], int *inoteElementSize,
						const char *cnotes, int *boolCnotesProvided,
						int *userHeader, int *userHeaderNumber,
						const char *units, const char *type, int *precisionValues,
						const char *timeZoneName,
						int *storageFlag, int *status,
						size_t pathLen, size_t baseDateLen, size_t cnotesLen,
						size_t unitsLen, size_t typeLen,
						size_t timeZoneNameLen)

{
	int ipos;
	int i;
	int len;
	int lengthCnotes;
	char *pathname;
	char *bDate;
	char *cunits;
	char *ctype;
	char *charNotes;

	zStructTimeSeries *tss;

	pathname = stringFortToC(path, pathLen);
	bDate = stringFortToC(baseDate, baseDateLen);
	cunits = stringFortToC(units, unitsLen);
	ctype = stringFortToC(type, typeLen);


	//  FIX ME - NEED TO accommodate TIME GRANULARITY!!!!
	if (*valueSize == 2) {
		tss = zstructTsNewIrregDoubles(pathname, (void *)values, *numberValues,  timeArray,
									   *timeArrayUnits, bDate, cunits, ctype);
	}
	else {
		tss = zstructTsNewIrregFloats(pathname, (void *)values, *numberValues,  timeArray,
									  *timeArrayUnits, bDate, cunits, ctype);
	}

	tss->precision = *precisionValues;
	if (timeZoneName && (timeZoneNameLen > 0)) {
		tss->timeZoneName = stringFortToC(timeZoneName, timeZoneNameLen);
		tss->allocated[zSTRUCT_timeZoneName] = 1;
	}

	if (*qualityElementSize) {
		tss->quality = quality;
		tss->qualityElementSize = *qualityElementSize;
	}

	if (*userHeaderNumber) {
		tss->userHeader = userHeader;
		tss->userHeaderNumber = *userHeaderNumber;
	}

	if ((*boolCnotesProvided != 0) && (*inoteElementSize > 0)) {
		////////////////////   FIX ME error
		*status = -1;
		return;
	}
	if (*boolCnotesProvided != 0) {
		tss->cnotesSize = (*numberValues) * (int)cnotesLen;
		charNotes = (char *)malloc((size_t)tss->cnotesSize);
		if (!charNotes) {
			*status = zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreIrreg_ID,
				zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
				zdssErrorSeverity.MEMORY_ERROR,
				path, "Allocating cnotes");
			return;
		}
		ipos = 0;
		for (i=0; i<*numberValues; i++) {
			len = strnlen_hec(&cnotes[i*cnotesLen], (int)cnotesLen);
			ipos += copyAndTrim(&charNotes[ipos], cnotesLen, &cnotes[i*cnotesLen], len);
			//  Include the null terminator  ??????????
			ipos++;
		}
		lengthCnotes = ipos;
		tss->cnotesLengthTotal = lengthCnotes;
		tss->cnotes = charNotes;
		tss->allocated[zSTRUCT_TS_cnotes] = 1;
	}
	else {
		if (*inoteElementSize) {
			tss->inotes = notes;
			tss->inoteElementSize = *inoteElementSize;
		}
	}

	*status = ztsStore(ifltab, tss, *storageFlag);

	zstructFree(tss);
	free(pathname);
	free(bDate);
	free(cunits);
	free(ctype);
}

