#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "hecdssFort.h"


//  FORTRAN Callable
//  call zsrtsd(ifltab1, cpath1, '25JUN1957', '2400', nvals, data1, 'CFS', 'PER-AVER', 0, status)
//  For compatibility purposes.   function is for storing only double values
void zsrtsd7_(long long *ifltab, const char *path,
				const char *startDate, const char *startTime,
				int *numberValues, int *values,
				const char *units, const char *type,
				int *storageFlag, int *status,
				size_t pathLen, size_t startDateLen,
				size_t startTimeLen, size_t unitsLen, size_t typeLen)
{
	int quality[1];
	int notes[1];
	int userHeader[1];
	int precision = -1;
	int zero = 0;
	int two = 2;

	char cdummy[1];

	ztsregstorefull_(ifltab, path,
					startDate, startTime, numberValues,
					values, &two,
					quality, &zero,
					notes, &zero,
					cdummy, &zero,
					userHeader, &zero,
					units, type, &precision,
					cdummy, storageFlag, status,
					pathLen, startDateLen, startTimeLen,
					zero, unitsLen, typeLen,
					zero);
}


//  FORTRAN Callable



void ztsregstorefull_(long long *ifltab, const char *path,
				const char *startDate, const char *startTime,
				int *numberValues,
				int values[], int *valueSize,
				int quality[], int *qualityElementSize,
				int notes[], int *inoteElementSize,
				const char *cnotes, int *boolCnotesProvided,
				int userHeader[], int *userHeaderNumber,
				const char *units, const char *type, int *precisionValues,
				const char *timeZoneName, int *storageFlag, int *status,
				size_t pathLen, size_t startDateLen, size_t startTimeLen,
				size_t cnotesLen, size_t unitsLen, size_t typeLen,
				size_t timeZoneLen)

{
	int ipos;
	int i;
	int len;
	int lengthCnotes;
	char *pathname;
	char *sDate;
	char *sTime;
	char *cunits;
	char *ctype;
	char ctimeZoneName[30];
	char *charNotes;

	zStructTimeSeries *tss;


	pathname = stringFortToC(path, pathLen);
	sDate = stringFortToC(startDate, startDateLen);
	sTime = stringFortToC(startTime, startTimeLen);
	cunits = stringFortToC(units, unitsLen);
	ctype = stringFortToC(type, typeLen);

	if (*valueSize == 2) {
		tss = zstructTsNewRegDoubles(pathname, (void *)values, *numberValues,
									(const char*)sDate, (const char*)sTime,
									cunits, ctype);
	}
	else {
		tss = zstructTsNewRegFloats(pathname, (void *)values, *numberValues,
									(const char*)sDate, (const char*)sTime,
									cunits, ctype);
	}

	free(pathname);
	free(sDate);
	free(sTime);
	free(cunits);
	free(ctype);

	if (!tss) {
		*status = zerrorProcessing(ifltab, DSS_FUNCTION_internalUtility_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
										zdssErrorSeverity.MEMORY_ERROR,
										path, "Allocating time series struct in ztsregstorefull_");
		return;
	}

	tss->precision = *precisionValues;
	if (timeZoneName && (timeZoneLen > 0)) {
		copyAndTrim(ctimeZoneName, sizeof(ctimeZoneName), timeZoneName, timeZoneLen);
		tss->timeZoneName = ctimeZoneName;
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
		tss->cnotesSize = (*numberValues) * (int)(cnotesLen);
		charNotes = (char *)malloc((size_t)tss->cnotesSize);
		if (!charNotes) {
			*status = zerrorProcessing(ifltab, DSS_FUNCTION_ztsStoreReg_ID,
				zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
				zdssErrorSeverity.MEMORY_ERROR,
				path, "Allocating cnotes");
			return;
		}
		ipos = 0;
		for (i=0; i<*numberValues; i++) {
			len = strnlen_hec(&cnotes[i*(int)cnotesLen], (int)cnotesLen);
			ipos += copyAndTrim(&charNotes[ipos], (size_t)cnotesLen, &cnotes[i*(int)cnotesLen], (size_t)len);
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


}


//  FORTRAN Callable
/*
      SUBROUTINE zsrtsi7 (IFLTAB, CPATH, CDATE, CTIME, NVALS,
     * LDOUBLE, SVALUES, DVALUES, JQUAL, LQUAL1, CUNITS, CTYPE,
     * IUHEAD, NUHEAD, COORDS, NCOORDS, ICDESC, NCDESC,
     * IPLAN, IGNORE1, IGNORE2, IGNORE3, IGNORE4, NPREC, ISTAT)
*/

//  call zsrts (ifltab1, cpath1, '25JUN1957', '2400', nvals, data1, 'CFS', 'PER-AVER', 0, status)
//  For compatibility purposes.  Preferred function is
//  limited to storing only real (floats) values

////   RENAME ME
void zsrtsi7_(long long *ifltab, const char *path,
				const char *startDate, const char *startTime,
				int *numberValues, int *boolDoubles,
				int *singleValues, int *doubleValues,
				int *quality, int *boolQuality,
				const char *units, const char *type,
				int *userHeader, int *userHeaderNumber,
				double coordinates[], int *numberCoordinates,
				int coordinateDescription[],  int *numbCoordDescription,
				int *storageFlag,
				int *ignore1, int *ignore2, int *ignore3, int *ignore4, int *nprec,
				int *status,
				size_t pathLen, size_t startDateLen,
				size_t startTimeLen, size_t unitsLen, size_t typeLen)
{

	int valueSize;
	int *values;
	int istat;
	int qualityElementSize;
	int notes[1];
	char cnotes[1];
	int zero = 0;
	int minusOne = -1;
	char timezone[1];


	if (*boolDoubles) {
		valueSize = 2;
		values = (int *)doubleValues;
	}
	else {
		valueSize = 1;
		values = (int *)singleValues;
	}

	if (*boolQuality) {
		qualityElementSize = 1;
	}
	else {
		qualityElementSize = 0;
	}


	ztsregstorefull_(ifltab, path,
					startDate, startTime,
					numberValues,
					values, &valueSize,
					quality, &qualityElementSize,
					notes, &zero,
					cnotes, &zero,
					userHeader, userHeaderNumber,
					units, type, &minusOne,
					timezone, storageFlag, status,
					pathLen, startDateLen, startTimeLen,
					zero, unitsLen, typeLen, zero);


	if (!*status) {
		if ((*numberCoordinates == 3) || (*numbCoordDescription == 6)) {
			timezone[0] = '\0';
			 zlocationstore_(ifltab, path,
						coordinates,
						coordinateDescription,
						timezone, timezone, &istat, pathLen, 0, 0);
		}
	}

}

