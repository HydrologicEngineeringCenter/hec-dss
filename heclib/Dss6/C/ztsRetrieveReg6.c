#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "heclib.h"
#include "zdssKeys.h"
#include "zdssVals.h"
//#include "heclib6.h"
#include "zStructTsTimeWindow.h"
#include "zStructTimeSeries.h"

/*

	Internal - Do NOT use!
	call ztsRetrieve() instead.


	Description

	start and end dates and times are optional.  If they are blank,
	then the full data set will be returned, unless a date range is
	given in the pathname {"20July2002 - 13May2010")

	retrieveFlag:	0:  Adhere to time window provided
					-1: Trim data.  Remove missing values at the beginning and end of data set (not inside)


		CALL zrrtsi6(IFLTAB, CPATH, CRDATE, CRTIME, KVALS, NVALS,
     *    LGETDOB, LFILDOB, SVALUES, DVALUES, JQUAL, LQUAL, LQREAD,
     *    CUNITS, CTYPE, IUHEAD, KUHEAD, NUHEAD, IOFSET, JCOMP,
     *    COORDS, ICDESC, LCOORDS, ISTAT)





void zrrtsi6_ (long long* ifltab, const char* pathname, const char* startDate,
			 const char* startTime, int* maxVals, int* numberVals,
			 int* getDoubles, int* doublesRead, float* floatValues, double* doubleValues,
			 int* flags, int* readFlags, int* flagsRead,
             char* units, char* type, int *userHeader, int *maxUserHead, int *numberUserHead,
			 int* offset, int* compression,
			 double* coordinates, int* coordinateDescription, int* coordinatesUsed,
			 int* status,
			 int lenPathname, int lenStartDate, int lenStartTime,
			 int lenUnits, int lenType);
			 */

int ztsRetrieveReg6(long long *ifltab, zStructTimeSeries *tss,
					int retrieveFlag, int retrieveDoublesFlag, int boolRetrieveQualityNotes)
{
	int mins;
	int kvals;
	int nvals;
	int lgetdob;
	int lfildob;
	float *svalues;
	double *dvalues;
	int *quality;
	int lqual;
	int lqread;
	int status;

	int userHeader[500];
	int kuserHeader;
	int nuserHeader;
	int ioffset;
	int jcomp;

	char cdummy[4];
	int precision;

	double coordinates[3];
	int coordinateDescription[6];
	int boolCoordinates;

	int ndata, lfound, idtype;
	char cdtype[30];


	char cdate[20];
	char ctime[10];
	char cunits[20];
	char ctype[20];
	char timezone[30];
	int itimezone;


	//  FIX ME - add time zone!!

	if (zgetVersion(ifltab) != 6) {
		//  Fix me - error out!
		return STATUS_NOT_OKAY;
	}


	julianToDate(tss->startJulianDate, 104, cdate, sizeof(cdate));
	mins = tss->startTimeSeconds / 60;
	minutesToHourMin(mins, ctime, sizeof(ctime));

	kvals = tss->numberValues;
	nvals = tss->numberValues;

	if (retrieveDoublesFlag == 0) {
		zdtype_(ifltab, zgetInternalPath(tss), &ndata, &lfound, cdtype, &idtype, strlen(zgetInternalPath(tss)), sizeof(cdtype));
		if (idtype == 105) {
			retrieveDoublesFlag = 2;
		}
	}

	if (retrieveDoublesFlag == 2) {
		lgetdob = 1;
		dvalues = (double *)malloc((size_t)kvals * 8);
		svalues = (float *)malloc(FLOAT_SIZE);
	}
	else {
		lgetdob = 0;
		dvalues = (double *)malloc(DOUBLE_SIZE);
		svalues = (float *)malloc((size_t)kvals * 4);
	}
	if (!dvalues || !svalues) {
		//  error out
		if (dvalues) free(dvalues);
		if (svalues) free(svalues);
		return STATUS_NOT_OKAY;
	}

	if (boolRetrieveQualityNotes) {
		lqual = 1;
		quality = (int *)malloc((size_t)kvals * 4);
		if (!quality) {
			//   error out
			return STATUS_NOT_OKAY;
		}
	}
	else {
		lqual = 0;
		quality = (int *)malloc(WORD_SIZE);
	}

	zeroFill(cunits, sizeof(cunits));
	zeroFill(ctype, sizeof(ctype));
	kuserHeader = 500;  //sizeof(userHeader);

	jcomp = 0;
	boolCoordinates = 1;

	zrrtsi6_(ifltab, zgetInternalPath(tss), cdate, ctime, &kvals, &nvals,
		&lgetdob, &lfildob, svalues, dvalues, quality, &lqual, &lqread,
		cunits, ctype, (int *)userHeader, &kuserHeader, &nuserHeader, &ioffset, &jcomp,
		coordinates, coordinateDescription, &boolCoordinates, &status,
		strlen(zgetInternalPath(tss)), strlen(cdate), strlen(ctime),
		sizeof(cunits), sizeof(ctype));

/*	SUBROUTINE zrrtsc6(IFLTAB, CPATH, CDATE, CTIME, KVALS, NVALS,
		*LGETDOB, LFILDOB, SVALUES, DVALUES, JQUAL, LQUAL, LQREAD,
		*CUNITS, CTYPE, CSUPP, IOFSET, JCOMP,
		*ITZONE, CTZONE, COORDS, ICDESC, LCOORDS, ISTAT)

/*VOID zrrtsi6_ (long long* ifltab, const char* pathname, const char* startDate,
			 const char* startTime, int* maxVals, int* numberVals,
			 int* getDoubles, int* doublesRead, float* floatValues, double* doubleValues,
			 int* flags, int* readFlags, int* flagsRead,
             char* units, char* type, int *userHeader, int *maxUserHead, int *numberUserHead,
			 int* offset, int* compression,
			 double* coordinates, int* coordinateDescription, int* coordinatesUsed,
			 int* status,
			 int lenPathname, int lenStartDate, int lenStartTime,
			 int lenUnits, int lenType)
	 */

	if ((status < 0) || (status > 4)) {
		if (dvalues) free(dvalues);
		if (svalues) free(svalues);
		if (quality) free(quality);
		if ((status > 3) && (status < 10)) {
			return STATUS_RECORD_NOT_FOUND;
		}
		return status;
	}

	zeroFill(timezone, sizeof(timezone));
	zgettz_(timezone, &itimezone, sizeof(timezone));
	if (timezone[0] != ' ') {
		tss->timeZoneName = stringFortToC(timezone, sizeof(timezone));
		tss->allocated[zSTRUCT_timeZoneName] = 1;
	}

	if (lgetdob) {
		tss->doubleValues = dvalues;
		tss->allocated[zSTRUCT_TS_doubleValues] = 1;
		if (svalues) free(svalues);
	}
	else {
		tss->floatValues = svalues;
		tss->allocated[zSTRUCT_TS_floatValues] = 1;
		if (dvalues) free(dvalues);
	}

	if (lqual) {
		if (lqread) {
			tss->quality = quality;
			tss->qualityElementSize = 1;
			tss->qualityArraySize = nvals;
			tss->allocated[zSTRUCT_TS_quality] = 1;
		}
		else {
			if (quality) free(quality);
		}
	}
	else {
		if (quality) free(quality);
	}

	tss->units = stringFortToC(cunits, sizeof(cunits));
	tss->allocated[zSTRUCT_TS_units] = 1;
	tss->type = stringFortToC(ctype, sizeof(ctype));
	tss->allocated[zSTRUCT_TS_type] = 1;

	if (nuserHeader > 0) {
		tss->userHeader = (int *)calloc(nuserHeader, 4);
		if (!tss->userHeader) {
			//  error out
			return STATUS_NOT_OKAY;
		}
		convertIntArray(userHeader, tss->userHeader, nuserHeader, 1, 1);
		tss->allocated[zSTRUCT_userHeader] = 1;
		tss->userHeaderSize = nuserHeader;
		tss->userHeaderNumber = nuserHeader;
	}

	//  Convert from minutes to seconds
	ioffset *= 60;
	tss->timeOffsetSeconds = ioffset;
	if (tss->timeWindow) tss->timeWindow->timeOffsetSeconds = ioffset;

	if (boolCoordinates) {
		tss->locationStruct->xOrdinate = coordinates[0];
		tss->locationStruct->yOrdinate = coordinates[1];
		tss->locationStruct->zOrdinate = coordinates[2];
		tss->locationStruct->coordinateSystem = coordinateDescription[0];
		tss->locationStruct->coordinateID = coordinateDescription[1];
		tss->locationStruct->horizontalUnits = coordinateDescription[2];
		tss->locationStruct->horizontalDatum = coordinateDescription[3];
		tss->locationStruct->verticalUnits = coordinateDescription[4];
		tss->locationStruct->verticalDatum = coordinateDescription[5];
	}

	zinqir6_(ifltab, "PREC", cdummy, &precision, 4, sizeof(cdummy));

	if (status == STATUS_RECORD_FOUND || status == STATUS_NO_OP)
	{
		long long *info = (long long*)ifltab[zdssKeys.kinfo];
		int ibuff[20];
		int istat = 0;
		zgetinfo6_(ifltab, tss->pathname, ibuff, &istat, strlen(tss->pathname));
		charLong(&ibuff[8], tss->programName, zdssVals.numberProgram, zdssVals.numberProgram, 0, 0);
	}

	tss->precision = precision;

	if (status < 4) {
		return STATUS_RECORD_FOUND;  //  Not status - we use differently now
	}
	else {
		return STATUS_RECORD_NOT_FOUND;
	}
}


