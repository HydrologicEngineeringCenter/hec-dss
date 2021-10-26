#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "heclib.h"
#include "zdssKeys.h"
#include "zdssVals.h"
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
					1:  Retrieve (one) value previous to start of time window for irregular interval
					2:  Retrieve (one) value after end of time window for irregular interval
					3:  Retrieve one value before and one value after time window for irregular interval
*/



int ztsRetrieveIrreg6(long long *ifltab, zStructTimeSeries *tss,
					int retrieveFlag, int retrieveDoublesFlag, int boolRetrieveQualityNotes)
{
	int minsStart;
	int minsEnd;
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
	int i;
	int baseDateMins;

	char userHeader[2000];
	int kuserHeader;
	int nuserHeader;

	double coordinates[3];
	int coordinateDescription[6];
	int boolCoordinates;

	char cdummy[4];
	int precision;

	char cunits[20];
	char ctype[20];
	char timezone[30];
	int itimezone;

	zStructRecordSize timeSeriesRecordSizes;


	//  FIX ME - add time zone!!

	if (zgetVersion(ifltab) != 6) {
		//  Fix me - error out!
		return STATUS_NOT_OKAY;
	}

	status = ztsGetSizes6(ifltab, tss, &timeSeriesRecordSizes);
	if (zisError(status)) {
		return status;
	}

	kvals = timeSeriesRecordSizes.numberValues;

	tss->times = (int *)malloc((size_t)kvals * 4);
	tss->allocated[zSTRUCT_TS_times] = 1;

	if (retrieveDoublesFlag == 0) {
		if (timeSeriesRecordSizes.dataType == 115) {
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
	zeroFill(userHeader, sizeof(userHeader));
	kuserHeader = sizeof(userHeader);
	boolCoordinates = 1;

	minsStart = tss->timeWindow->startTimeSeconds / 60;
	minsEnd = tss->timeWindow->endTimeSeconds / 60;

	zritsi6_(ifltab, zgetInternalPath(tss), &tss->timeWindow->startJulian, &minsStart,
		&tss->timeWindow->endJulian, &minsEnd, &lgetdob, &lfildob, tss->times,
		svalues, dvalues, &kvals, &nvals, &tss->julianBaseDate,
		quality, &lqual, &lqread, cunits, ctype,
		(int *)userHeader, &kuserHeader, &nuserHeader,
		coordinates, coordinateDescription, &boolCoordinates,
		&retrieveFlag, &status,
		strlen(zgetInternalPath(tss)), sizeof(cunits), sizeof(ctype));

	//  status == 4 means no data found; > 4 is an error
	if ((status < 0) || (status >= 4)) {
		if (dvalues) free(dvalues);
		if (svalues) free(svalues);
		if (quality) free(quality);
		if (status == 4) return STATUS_RECORD_NOT_FOUND;
		return status;
	}

	tss->numberValues = nvals;

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
		tss->dataType = 115;
		tss->sizeEachValueRead = 2;
	}
	else {
		tss->floatValues = svalues;
		tss->allocated[zSTRUCT_TS_floatValues] = 1;
		if (dvalues) free(dvalues);
		tss->dataType = 110;
		tss->sizeEachValueRead = 1;
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
		tss->userHeader = (int *)calloc((size_t)(nuserHeader + 1), 4);
		if (!tss->userHeader) {
			//  error out
			return STATUS_NOT_OKAY;
		}
		for (i=0; i<nuserHeader; i++) {
			tss->userHeader[i] = userHeader[i];
		}
		tss->allocated[zSTRUCT_userHeader] = 1;
		tss->userHeaderSize = nuserHeader;
		tss->userHeaderNumber = nuserHeader;
	}


	if (tss->julianBaseDate != 0) {
		//  Accomidates old Java code, which ignores this
		baseDateMins = tss->julianBaseDate * 1440;
		for (i=0; i<tss->numberValues; i++) {
			tss->times[i] += baseDateMins;
		}
		tss->julianBaseDate = 0;
	}

	zinqir6_(ifltab, "PREC", cdummy, &precision, 4, sizeof(cdummy));
	readProgramName(ifltab, tss, status);
	tss->precision = precision;


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

	return STATUS_RECORD_FOUND;
}

