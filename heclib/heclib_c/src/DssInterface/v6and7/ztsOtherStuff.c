#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "zdssKeys.h"
#include "zdssMessages.h"
#include "heclib.h"
#include "heclib6.h"
#include "zStructTsTimeWindow.h"


float zmissingFlag();


//  FORTRAN Interface
/*
void ztsretrieveregfull_(long long *ifltab, const char *path,
				const char *startDate, const char *startTime,
				int *maxNumberValues, int *numberRead, int *timeOffsetSeconds,
				int values[], int *sizeEachValueRequested, int *sizeEachValueRead,
				int quality[], int *qualityElementSizeRequested, int *qualityElementSizeRead,
				int notes[], int *inoteElementSizeRequested, int *sizeEachNoteRead,
				char *cnotesFort, int *totalSizeCNotes, int *totalNumberCnotesRead,
				int userHeader[], int *userHeaderArraySize, int *userHeaderNumber,
				char *units, char *type, int *precisionValues,
				double coordinates[], int *maxCoordinates, int *numberCoordinates,
				int coordinateDescription[], int *maxCoordDescription, int *numbCoordDescription,
				char *timeZoneName, int *timeZoneOffsetMinutes, int *status,
				int pathLen, int startDateLen, int startTimeLen, int cNotesLen,
				int unitsLen, int typeLen, int timeZoneLen)


{

	char pathname[MAX_PATHNAME_LENGTH];
	char sDate[20];
	char sTime[10];
	char cunits[13];
	char ctype[13];
	char ctimeZoneName[30];
	char *cnotes;
	int ipos;
	int jpos;
	int len;
	int lencnotesRead;
	int i;

	copyAndTrim(pathname, sizeof(pathname), path, pathLen);
	copyAndTrim(sDate, sizeof(sDate), startDate, startDateLen);
	copyAndTrim(sTime, sizeof(sTime), startTime, startTimeLen);

	if (*totalSizeCNotes > 0) {
		cnotes = (char *)malloc((size_t)*totalSizeCNotes);
	}
	else {
		cnotes = 0;
	}

	*status = ztsRegRetrieveFull7(ifltab, pathname,
					sDate, sTime,
					*maxNumberValues, numberRead, timeOffsetSeconds,
					values, *sizeEachValueRequested, sizeEachValueRead,
					quality, *qualityElementSizeRequested, qualityElementSizeRead,
					notes, *inoteElementSizeRequested, sizeEachNoteRead,
					cnotes, *totalSizeCNotes, totalNumberCnotesRead,
					userHeader, *userHeaderArraySize, userHeaderNumber,
					cunits, sizeof(cunits), ctype, sizeof(ctype), precisionValues,
					coordinates, *maxCoordinates, numberCoordinates,
					coordinateDescription,  *maxCoordDescription, numbCoordDescription,
					ctimeZoneName, sizeof(ctimeZoneName), timeZoneOffsetMinutes);

	if (*status >= 0) {
		stringCToFort(units, unitsLen,  cunits);
		stringCToFort(type, typeLen,  ctype));
		stringCToFort(timeZoneName, timeZoneLen,  ctimeZoneName);
		if ((*totalNumberCnotesRead > 0) && (cnotes != 0)){
			ipos = 0;
			jpos = 0;
			lencnotesRead = *totalNumberCnotesRead;
			for (i=0; i<*numberRead; i++) {
				len = strnlen(&cnotes[jpos], (size_t)lencnotesRead);
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
}
*/

//  FORTRAN Callable
//  call zrrtsd7(ifltab1, cpath1, '25JUN1957', '2400', nvals, data1, 'CFS', 'PER-AVER', 0, status)
//  For compatibility purposes.   function is  for retrieving only double values
//zrrtsd ( IFLTAB, CPATH, CDATE, CTIME, NVALS, VALUES, CUNITS, CTYPE, IOFSET, ISTAT)
void zrrtsd7_(long long *ifltab, const char *path,
				const char *startDate, const char *startTime,
				int *maxNumberValues, int *numberRead, int *values,
				char *units, char *type,
				int *timeOffsetSeconds, int *status,
				size_t pathLen, size_t startDateLen,
				size_t startTimeLen, size_t unitsLen, size_t typeLen)

{
	int quality[1];
	int notes[1];
	int userHeader[1];
	int precisionValues;

	int sizeEachValueRequested;
	int zero = 0;

	char ctimeZoneName[30];
	char cnotes[1];

	int sizeEachValueRead;
	int qualityElementSizeRead;
	int sizeEachNoteRead;
	int totalNumberCnotesRead;
	int userHeaderNumber;

	sizeEachValueRequested = 2;
	zero = 0;


	*status = ztsRetrieveRegArgs(ifltab, path,
				startDate, startTime, timeOffsetSeconds,
				*maxNumberValues, numberRead,
				values, sizeEachValueRequested, &sizeEachValueRead,
				quality, zero, &qualityElementSizeRead,
				notes, zero, &sizeEachNoteRead,
				cnotes, zero, &totalNumberCnotesRead,
				userHeader, zero, &userHeaderNumber,
				units, sizeof(units), type, sizeof(type), &precisionValues,
				ctimeZoneName, sizeof(ctimeZoneName));



/*
	copyAndTrim(pathname, sizeof(pathname), path, pathLen);
	copyAndTrim(sDate, sizeof(sDate), startDate, startDateLen);
	copyAndTrim(sTime, sizeof(sTime), startTime, startTimeLen);

	sizeEachValueRequested = 2;

	*status = ztsRegRetrieveFull7(ifltab, pathname,
					sDate, sTime,
					*maxNumberValues, numberRead, offset,
					values, 2, &sizeEachValueRead,
					quality, 0, &qualityElementSizeRead,
					notes, 0, &sizeEachNoteRead,
					cnotes, 0, &totalNumberCnotesRead,
					userHeader, 0,  &zero,
					cunits, sizeof(cunits), ctype, sizeof(ctype), &precisionValues,
					coordinates, 0,  &zero,
					coordinateDescription,  0,  &zero,
					ctimeZoneName, sizeof(ctimeZoneName),  &zero);

	if (*status >= 0) {
		stringCToFort(units, unitsLen,  cunits);
		stringCToFort(type, typeLen,  ctype);
	}
	else {
		*numberRead = 0;
	}
*/
}

//  FORTRAN Callable
//  call zrrts7(ifltab1, cpath1, '25JUN1957', '2400', nvals, data1, 'CFS', 'PER-AVER', 0, status)
//  For compatibility purposes.   function is  for retrieving only single values
//zrrts ( IFLTAB, CPATH, CDATE, CTIME, NVALS, VALUES, CUNITS, CTYPE, IOFSET, ISTAT)
void zrrts7_(long long *ifltab, const char *path,
				const char *startDate, const char *startTime,
				int *maxNumberValues, int *numberRead, int *values,
				char *units, char *type,
				int *timeOffsetSeconds, int *status,
				size_t pathLen, size_t startDateLen,
				size_t startTimeLen, size_t unitsLen, size_t typeLen)

{
	int quality[1];
	int notes[1];
	int userHeader[1];
	int precisionValues;

	int sizeEachValueRequested;
	int zero = 0;

	char ctimeZoneName[30];
	char cnotes[1];

	int sizeEachValueRead;
	int qualityElementSizeRead;
	int sizeEachNoteRead;
	int totalNumberCnotesRead;
	int userHeaderNumber;

	sizeEachValueRequested = 1;
	zero = 0;


	*status = ztsRetrieveRegArgs(ifltab, path,
				startDate, startTime, timeOffsetSeconds,
				*maxNumberValues, numberRead,
				values, sizeEachValueRequested, &sizeEachValueRead,
				quality, zero, &qualityElementSizeRead,
				notes, zero, &sizeEachNoteRead,
				cnotes, zero, &totalNumberCnotesRead,
				userHeader, zero, &userHeaderNumber,
				units, sizeof(units), type, sizeof(type), &precisionValues,
				ctimeZoneName, sizeof(ctimeZoneName));;


}

/*
      SUBROUTINE zrrtsi7 (IFLTAB, CPATH, CRDATE, CRTIME, KVALS, NVALS,
     * LGETDOB, LFILDOB, SVALUES, DVALUES, JQUAL, LQUAL, LQREAD,
     * CUNITS, CTYPE, IUHEAD, KUHEAD, NUHEAD, IOFSET, JCOMP,
     * COORDS, ICDESC, LCOORDS, ISTAT)
*/
//  FORTRAN Callable
//  For compatibility purposes.

void zrrtsi7_(long long *ifltab, const char *path,
				const char *startDate, const char *startTime,
				int *numberToRead, int *numberRead,
				int *boolGetDoubles, int *boolDoublesRead,
				int *singles, int *doubles,
				int *quality, int *boolGetQuality, int *boolQualityRead,
				char *units, char *type,
				int *userHeader, int *userHeaderArraySize, int *numberUserHeaderRead,
				int *offset,
				int *compressionNotUsed,
				double coordinates[], int coordinateDescription[], int *boolCoordinates,
				int *istat,
				size_t pathLen, size_t startDateLen,
				size_t startTimeLen, size_t unitsLen, size_t typeLen)

{
	int zero = 0;
	int status;
	int i;

	char *pathname;
	char *sDate;
	char *sTime;
	char tzName[24];

	zStructTimeSeries *tss;

	int len;
	int retrieveDoublesFlag;

	float missing;


	pathname = stringFortToC(path, pathLen);
	sDate = stringFortToC(startDate, startDateLen);
	sTime = stringFortToC(startTime, startTimeLen);

	if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebug(ifltab, DSS_FUNCTION_other_ID, "zrrtsi7_, Pathname: ", pathname);
		zmessageDebug(ifltab, DSS_FUNCTION_other_ID, "zrrtsi7_, Time window prior to processing: ", "");
		zmessageDebug(ifltab, DSS_FUNCTION_other_ID, "zrrtsi7_, Start: ", sDate);
		zmessageDebugInt(ifltab, DSS_FUNCTION_other_ID, "zrrtsi7_, Number to read: ", *numberToRead);
	}



	tss = zstructTsNewTimes(pathname, sDate, sTime, "", "");
	free(pathname);
	free(sDate);
	free(sTime);
	tss->numberValues = *numberToRead;
	retrieveDoublesFlag = 1;
	if (*boolGetDoubles) retrieveDoublesFlag = 2;
	status = ztsRetrieve(ifltab, tss, -2, retrieveDoublesFlag, *boolGetQuality);


	if (status == STATUS_RECORD_FOUND) {
		*istat = 0;
		*numberRead = tss->numberValues;

		if (*boolGetDoubles) {
			if (tss->doubleValues) {
				convertDataArray((void *)tss->doubleValues, doubles, tss->numberValues, 2, 2);
			}
			else if (tss->floatValues) {
				convertDataArray((void *)tss->floatValues, doubles, tss->numberValues, 1, 2);
			}
			else {
				//  Nasty error here
			}
			if (tss->sizeEachValueRead == 2) {
				*boolDoublesRead = 1;
			}
			else {
				*boolDoublesRead = 0;
			}
		}
		else {
			if (tss->doubleValues) {
				convertDataArray((void *)tss->doubleValues, singles, tss->numberValues, 2, 1);
				*boolDoublesRead = 1;
			}
			else if (tss->floatValues) {
				convertDataArray((void *)tss->floatValues, singles, tss->numberValues, 1, 1);
				*boolDoublesRead = 0;
			}
			else {
				//  Nasty error here
			}
		}

		if (*boolGetQuality) {
			if (tss->qualityElementSize > 0) {
				//  Cannot handle anything other than single quality??
				for (i=0; i<tss->numberValues; i++) {
					quality[i] = tss->quality[i * tss->qualityElementSize];
				}
				*boolQualityRead = 1;
			}
			else {
				*boolQualityRead = 0;
			}
		}
		else {
			*boolQualityRead = 0;
		}

		stringCToFort(units, unitsLen,  tss->units);
		stringCToFort(type, typeLen,  tss->type);

		if ((*userHeaderArraySize > 0) && (tss->userHeaderNumber > 0)) {
			len = tss->userHeaderNumber;
			if (*userHeaderArraySize < len) len = *userHeaderArraySize;
			for (i=0; i<len; i++) {
				userHeader[i] = tss->userHeader[i];
			}
			*numberUserHeaderRead = len;
		}
		else {
			*numberUserHeaderRead = 0;
		}

		*offset = tss->timeOffsetSeconds / SECS_IN_1_MINUTE;
		*compressionNotUsed = 0;


		if (tss->locationStruct) {
			coordinates[0] = tss->locationStruct->xOrdinate;
			coordinates[1] = tss->locationStruct->yOrdinate;
			coordinates[2] = tss->locationStruct->zOrdinate;
			coordinateDescription[0] = tss->locationStruct->coordinateSystem;
			coordinateDescription[1] = tss->locationStruct->coordinateID;
			coordinateDescription[2] = tss->locationStruct->horizontalUnits;
			coordinateDescription[3] = tss->locationStruct->horizontalDatum;
			coordinateDescription[4] = tss->locationStruct->verticalUnits;
			coordinateDescription[5] = tss->locationStruct->verticalDatum;
			*boolCoordinates = 1;
		}
		else {
			//////////////////////    TEMP
			*boolCoordinates = 0;
		}

		if (tss->timeZoneName) {
			stringCToFort(tzName, sizeof(tzName)-1, tss->timeZoneName);
			zsettimezone6_(&zero, tzName, &tss->precision, strlen(tss->timeZoneName));
		}
		else {
			zsettimezone6_(&zero, "\0", &tss->precision, 0);
		}


	}
	else {
		//  FIX ME here to correspond to old style
		if (zisError(status)) {
			*istat = 10;
		}
		else {
			*istat = 5;
		}
		missing = zmissingFlag();
		if (*boolGetDoubles) {
			fillArray((void *)&missing, 1, doubles,  2, *numberToRead);
		}
		else {
			fillArray((void *)&missing, 1, singles,  1, *numberToRead);
		}
		*boolDoublesRead = 0;
	}

	zstructFree(tss);

}

