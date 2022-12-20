#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "zStructTsTimeWindow.h"
#include "fortran_string_len_size.h"


/*
      SUBROUTINE zritsi7 (IFLTAB, CPATH, JULS, ISTIME, JULE, IETIME,
     * LGETDOB, LFILDOB, ITIMES, SVALUES, DVALUES, KVALS, NVALS,
     * IBDATE, IQUAL, LQUAL, LQREAD, CUNITS, CTYPE, IUHEAD, KUHEAD,
     * NUHEAD, COORDS, ICDESC, LCOORDS, INFLAG, ISTAT)
*/
//  FORTRAN Callable
//  For compability purposes.



				//////////////////////////////
				////  TODO - Move interface functions into separate dirctory
				///  Make a #Define DSS6 that calls equivalent Dss 6 version if available
				/////////////////////////////

void zritsi7_(long long *ifltab, const char *path,
				int *startJulian, int *startTimeMinutes,
				int *endJulian, int *endTimeMinutes,
				int *boolGetDoubles, int *boolDoublesRead,
				int *timeArray, int *singles, int *doubles,
				int *maxNumberValues, int *numberRead,
				int *julianBaseDate,
				int *quality, int *boolGetQuality, int *boolQualityRead,
				char *units, char *type,
				int *userHeader, int *userHeaderArraySize, int *userHeaderNumber,
				double coordinates[], int coordinateDescription[], int *boolCoordinatesRead,
				int *readFlag, int *status,
				slen_t pathLen, slen_t unitsLen, slen_t typeLen)
{

	int i;
	int zero = 0;
	char *pathname;

	int retrieveDoublesFlag;
	int len;

	zStructTimeSeries *tss;


	pathname = stringFortToC(path, pathLen);
	tss = zstructTsNew(pathname);
	free(pathname);
	if ((*startJulian == 0) && (*endJulian == 0)) {
		//  Times not defined (use pathname date part)
	}
	else {
		tss->startJulianDate = *startJulian;
		tss->startTimeSeconds = *startTimeMinutes * 60;
		tss->endJulianDate = *endJulian;
		tss->endTimeSeconds = *endTimeMinutes * 60;
	}
	retrieveDoublesFlag = 1;
	if (*boolGetDoubles) retrieveDoublesFlag = 2;
	*status = ztsRetrieve(ifltab, tss, *readFlag, retrieveDoublesFlag, *boolGetQuality);



	if (*status >= 0) {
		if (tss->numberValues > *maxNumberValues) {
			tss->numberValues = *maxNumberValues;
		}
		*numberRead = tss->numberValues;
		for (i=0; i<tss->numberValues; i++) {
			timeArray[i] = tss->times[i];
		}
		*julianBaseDate = tss->julianBaseDate;
		/* if (tss->julianBaseDate != 0) {
			//  Accomidates old Java code, which ignores this
			baseDateMins = tss->julianBaseDate * 1440;
			for (i=0; i<tss->numberValues; i++) {
				timeArray[i] =- baseDateMins;
			}
		} */

		if (*boolGetDoubles) {
			if (tss->doubleValues) {
				convertDataArray((void *)tss->doubleValues, (void *)doubles, tss->numberValues, 2, 2);
			}
			else if (tss->floatValues) {
				convertDataArray((void *)tss->floatValues, (void *)doubles, tss->numberValues, 1, 2);
			}
			else {
				//  Nasty error here
			}
		}
		else {
			if (tss->doubleValues) {
				convertDataArray((void *)tss->doubleValues, (void *)singles, tss->numberValues, 2, 1);
			}
			else if (tss->floatValues) {
				convertDataArray((void *)tss->floatValues, (void *)singles, tss->numberValues, 1, 1);
			}
			else {
				//  Nasty error here
			}
		}

		if (*boolGetQuality) {
			if (tss->qualityElementSize > 0) {
				//  Cannot handle anything other than single quality??
				convertIntArray((void *)tss->quality, (void *)quality, tss->numberValues, tss->qualityElementSize, 1);
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

	//	stringCToFort(timeZoneName, timeZoneNameLen,  tss->timeZoneName);

		if ((*userHeaderArraySize > 0) && (tss->userHeaderNumber > 0)) {
			len = tss->userHeaderNumber;
			if (*userHeaderArraySize < len) len = *userHeaderArraySize;
			for (i=0; i<len; i++) {
				userHeader[i] = tss->userHeader[i];
			}
			*userHeaderNumber = len;
		}
		else {
			*userHeaderNumber = 0;
		}
	}


	zstructFree(tss);


}

