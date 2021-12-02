#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib.h"
#include "hecdssInternal.h"
/*
      SUBROUTINE ZRPDI6 (IFLTAB, CPATH, NORD, NCURVE, IHORIZ,
     * C1UNIT, C1TYPE, C2UNIT, C2TYPE, SVALUES, DVALUES,
     * LDOUBLE, KVALS, NVALS, CLABEL, KLABEL, LABEL,
     * IUHEAD, KUHEAD, NUHEAD, ISTAT)
	 void zrpdi6_(long long *ifltab, const char *pathname,
			 int *numberOrdinates, int *numberCurves, int *iHorizontal,
			 char *unitsIndependent, char *typeIndependent,
			 char *unitsDependent, char *typeDependent,
			 int *svalues, int *dvalues, int *boolDoubles, int *maxValues, int *numberValues,
			 char *labels, int *maxLables, int *boolLabels,
			 int *userHeader, int *maxUserHead, int *userHeaderNumber,
			 int *istat,
			 size_t pathnameLen,
			 size_t unitsIndependentLen, size_t typeIndependentLen,
			 size_t unitsDependentLen, size_t typeDependentLen,
			 size_t lablesLength);
	 */

int zpdRetrieve6(long long *ifltab, zStructPairedData *pds, int retrieveSizeFlag)
{
	int i, j;
	int ipos;
	int len;
	int labels;
	int status;
	float  *svalues;
	double *dvalues;
	int number;
	int maxValues;
	int boolDouble;
	int hundred;
	int userHeaderNumber;
	char unitsIndependent[100];
	char typeIndependent[100];
	char unitsDependent[100];
	char typeDependent[100];
	char clabel[100][101];
	zStructRecordSize *recordSize;


	//  Check for correct DSS Version
	if (zgetVersion(ifltab) != 6) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
								0, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, pds->pathname, "");
	}

	hundred = 100;

	recordSize = zstructRecordSizeNew(pds->pathname);
	if (!recordSize) {
		return -1;
	}

	status = zgetRecordSize6(ifltab, recordSize);
	if (status != STATUS_RECORD_FOUND) {
		return -1;
	}

	pds->dataType = recordSize->dataType;
	if ((recordSize->dataType < DATA_TYPE_PD) || (recordSize->dataType >= DATA_TYPE_TEXT)) {
		status = zerrorProcessing(ifltab, DSS_FUNCTION_zpdRetrieve_ID,
			zdssErrorCodes.WRONG_RECORD_TYPE, DATA_TYPE_PD,
			(long long)recordSize->dataType, zdssErrorSeverity.WARNING, recordSize->pathname, "");
		zstructFree(recordSize);
		return status;
	}

	pds->lastWrittenTime = recordSize->lastWriteTimeMillis;
	pds->sizeEachValueRead = recordSize->ipdValueSize;

	//  Allocate space needed
	maxValues = recordSize->pdNumberOrdinates * (recordSize->pdNumberCurves + 1);
	
	if (retrieveSizeFlag == 2) {
		boolDouble = 1;
	}
	else if (retrieveSizeFlag == 1) {
		boolDouble = 0;
	}
	else {
		if (recordSize->ipdValueSize == 2) {
			boolDouble = 1;
		}
		else {
			boolDouble = 0;
		}
	}
	if (boolDouble) {
		if ((recordSize->values1Number/2) > maxValues) {
			maxValues = (recordSize->values1Number/2) + 1;
		}
		dvalues = (double *)calloc(maxValues, 8);
		svalues = 0;
	}
	else {
		if (recordSize->values1Number > maxValues) {
			maxValues = recordSize->values1Number;
		}
		svalues = (float *)calloc(maxValues, 4);
		dvalues = 0;
	}

	if (recordSize->userHeaderNumber > 0) {
		userHeaderNumber = recordSize->userHeaderNumber + 75; // make sure there's enough room for vertical datum info
		pds->userHeader = (int *)calloc(userHeaderNumber, 4);
		pds->allocated[zSTRUCT_userHeader] = 1;
	}
	else {
		userHeaderNumber = 0;
	}

	for (i=0; i<sizeof(unitsIndependent); i++) unitsIndependent[i] = ' ';
	for (i=0; i<sizeof(typeIndependent); i++) typeIndependent[i] = ' ';
	for (i=0; i<sizeof(unitsDependent); i++) unitsDependent[i] = ' ';
	for (i=0; i<sizeof(typeDependent); i++) typeDependent[i] = ' ';
	for  (i=0; i<hundred; i++) {
		for (j=0; j<sizeof(clabel[i]); j++) clabel[i][j] = ' ';
	}


	if (recordSize->pdPrecision < 0) {
		pds->xprecision = -1;
		pds->yprecision = -1;
	}
	else {
		pds->xprecision = recordSize->pdPrecision / 10;
		pds->yprecision = recordSize->pdPrecision - (pds->xprecision  * 10);
	}

	zstructFree(recordSize);
	recordSize = 0;


	//  Call the function
	zrpdi6_(ifltab, pds->pathname, &pds->numberOrdinates, &pds->numberCurves, &pds->boolIndependentIsXaxis,
		unitsIndependent, typeIndependent, unitsDependent, typeDependent,
		svalues, dvalues, &boolDouble, &maxValues, &number,
		(char *)clabel, &hundred, &labels, pds->userHeader, &userHeaderNumber, &pds->userHeaderNumber, &status,
		strlen(pds->pathname), sizeof(unitsIndependent), sizeof(typeIndependent),
        sizeof(unitsDependent), sizeof(typeDependent), sizeof(clabel[0]));

	//  Error during initial read?
	if (status) {
		return status;
	}

	pds->numberCurvesInStruct = pds->numberCurves;
	pds->numberOrdinatesInStruct = pds->numberOrdinates;
	if ((pds->endingCurve == 0) && (pds->endingOrdinate == 0)) {
		pds->startingCurve = 1;
		pds->endingCurve = pds->numberCurves;
		pds->startingOrdinate = 1;
		pds->endingOrdinate = pds->numberOrdinates;
	}
	number = pds->numberOrdinates * pds->numberCurves;
	if (boolDouble) {
		pds->doubleOrdinates = (double *)calloc(pds->numberOrdinates, 8);
		pds->doubleValues    = (double *)calloc(number, 8);
		convertDataArray((void *)dvalues, (void *)pds->doubleOrdinates, pds->numberOrdinates, 2, 2);
		ipos = pds->numberOrdinates;
		convertDataArray((void *)&dvalues[ipos], (void *)pds->doubleValues, number, 2, 2);
		pds->allocated[zSTRUCT_PD_doubleOridnates] = 1;
		pds->allocated[zSTRUCT_PD_doubleValues] = 1;
		free(dvalues);
		dvalues = 0;
	}
	else {
		pds->floatOrdinates = (float *)calloc(pds->numberOrdinates, 4);
		pds->floatValues    = (float *)calloc(number, 4);
		convertDataArray((void *)svalues, (void *)pds->floatOrdinates, pds->numberOrdinates, 1, 1);
		ipos = pds->numberOrdinates;
		convertDataArray((void *)&svalues[ipos], (void *)pds->floatValues, number, 1, 1);
		pds->allocated[zSTRUCT_PD_floatOrdinates] = 1;
		pds->allocated[zSTRUCT_PD_floatValues] = 1;
		free(svalues);
		svalues = 0;
	}

	pds->unitsIndependent = stringFortToC(unitsIndependent, sizeof(unitsIndependent));
	pds->typeIndependent  = stringFortToC(typeIndependent, sizeof(typeIndependent));
	pds->unitsDependent   = stringFortToC(unitsDependent, sizeof(unitsDependent));
	pds->typeDependent    = stringFortToC(typeDependent, sizeof(typeDependent));
	pds->allocated[zSTRUCT_unitsIndependent] = 1;
	pds->allocated[zSTRUCT_typeIndependent] = 1;
	pds->allocated[zSTRUCT_unitsDependent] = 1;
	pds->allocated[zSTRUCT_typeDependent] = 1;

	if (labels) {
		number = 0;
		for  (i=0; i<pds->numberCurves; i++) {
			chrlnb_(clabel[i], &len, sizeof(clabel[i]));
			number += len + 1;
		}
		pds->labels = (char *)calloc(number, 1);
		pds->allocated[zSTRUCT_PD_labels] = 1;
		pds->labelsLength = number;
		ipos = 0;
		for  (i=0; i<pds->numberCurves; i++) {
			chrlnb_(clabel[i], &len, sizeof(clabel[i]));
			stringCopy(&pds->labels[ipos],(number-ipos), clabel[i], len);
			ipos += len;
			pds->labels[ipos++] = '\0';
		}
	}

	return status;
}



