#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "heclib6.h"
#include "hecdssInternal.h"
#include "verticalDatum.h"
/*
CALL ZSPDI6 (IFLTAB, CPATH, NORD, NCURVE, IHORIZ,
     *    C1UNIT, C1TYPE, C2UNIT, C2TYPE, SVALUES, DVALUES,
     *    LDOUBLE, CLABEL, LABEL, IUHEAD, NUHEAD, IPLAN, ISTAT)
	 */

int zpdStore6(long long *ifltab, zStructPairedData *pds, int storageFlag)
{
	int i, j;
	int ipos;
	int jpos;
	int len;
	int labels;
	int status;
	float  *svalues;
	double *dvalues;
	int number;
	int precision;
	int boolDouble;
	char clabel[100][101];
	char clab[101];

	char *unitsIndependent;
	char *typeIndependent;
	char *unitsDependent;
	char *typeDependent;
	char cdummy[5];



	//  Check for correct DSS Version
	if (zgetVersion(ifltab) != 6) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zpdStore_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
								0, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, pds->pathname, "");
	}


		//  Be sure we pass in valid char pointers
	if (pds->unitsIndependent) {
		unitsIndependent = pds->unitsIndependent;
	}
	else {
		cdummy[0] = '\0';
		unitsIndependent = cdummy;
	}
	if (pds->typeIndependent) {
		typeIndependent = pds->typeIndependent;
	}
	else {
		cdummy[0] = '\0';
		typeIndependent = cdummy;
	}
	if (pds->unitsDependent) {
		unitsDependent = pds->unitsDependent;
	}
	else {
		cdummy[0] = '\0';
		unitsDependent = cdummy;
	}
	if (pds->typeDependent) {
		typeDependent = pds->typeDependent;
	}
	else {
		cdummy[0] = '\0';
		typeDependent = cdummy;
	}


	//  are we storing a single curve in a family of curves?
	if (pds->numberCurves == 0) {
		if (pds->floatValues) {
			boolDouble = 0;
		}
		else if (pds->doubleValues) {
			boolDouble = 1;
		}
		else {
		}
		if (pds->labelsLength > 0) {
			labels = 1;
			for (j=0; j<101; j++) clab[j] = ' ';
			len = (int)strlen(pds->labels);
			if (len > 100) len = 100;
			for (j=0; j<len; j++) clab[j] = pds->labels[j];
		}
		else {
			labels = 0;
			len = 0;
		}
		storageFlag = 11;
		//  The curve number needs to be given (starting at 1)
		zspdi6_(ifltab, pds->pathname, &pds->numberOrdinates, &pds->startingCurve, &pds->boolIndependentIsXaxis,
			unitsIndependent, typeIndependent, unitsDependent, typeDependent,
			(void *)pds->floatValues, (void *)pds->doubleValues, &boolDouble,
			(const char *)clab, &labels, pds->userHeader, &pds->userHeaderNumber,
			&storageFlag, &status,
			strlen(pds->pathname), strlen(unitsIndependent), strlen(typeIndependent),
			strlen(unitsDependent), strlen(typeDependent), len);
		return status;
	}


	if (storageFlag >= 10) {
		storageFlag = 10;  // switch to version 6 flags
		number = pds->numberOrdinates;  //  Only store ordinates for this call
		svalues = pds->floatOrdinates;
		dvalues = pds->doubleOrdinates;
		if (pds->floatOrdinates) {
			boolDouble = 0;
		}
		else {
			boolDouble = 1;
		}
	}
	else {
	//  no, regular store
		number = pds->numberOrdinates * (pds->numberCurves + 1);
		if (pds->floatOrdinates) {
			svalues = (float *)calloc((size_t)number, 4);
			convertDataArray((void *)pds->floatOrdinates, (void *)svalues,  pds->numberOrdinates, 1, 1);
			number = pds->numberOrdinates * pds->numberCurves;
			convertDataArray((void *)pds->floatValues, (void *)(&svalues[pds->numberOrdinates]),  number, 1, 1);
			dvalues = 0;
			boolDouble = 0;
		}
		else if (pds->doubleOrdinates) {
			dvalues = (double *)calloc((size_t)number, 8);
			convertDataArray((void *)pds->doubleOrdinates, (void *)dvalues,  pds->numberOrdinates, 2, 2);
			number = pds->numberOrdinates * pds->numberCurves;
			convertDataArray((void *)pds->doubleValues, (void *)(&dvalues[pds->numberOrdinates]),  number, 2, 2);
			svalues = 0;
			boolDouble = 1;
		}
		else {
		}
	}

	if (pds->labelsLength > 0) {
		labels = 1;
		ipos = 0;
		for  (i=0; i<pds->numberCurves; i++) {
			if (ipos >= pds->labelsLength) break;
			len = (int)strlen(&pds->labels[ipos]);
			jpos = ipos;
			ipos += len;
			if (len > sizeof(clabel[0]))
				len = sizeof(clabel[0]);
			for (j=0; j<101; j++) clabel[i][j] = ' ';
			for (j=0; j<len; j++) clabel[i][j] = pds->labels[jpos++];
			ipos++;
		}
	}
	else {
		labels = 0;
	}

	if (pds->xprecision > 0) {
		precision = (pds->xprecision * 10) + pds->yprecision;
	}
	else {
		precision = -1;
	}
	zset6_("PREC", "", &precision, 4, 0);


	int *userHeader = (int *)calloc(pds->userHeaderNumber, 4);
	memcpy(userHeader, pds->userHeader, pds->userHeaderNumber * 4);
	if (getEndian()) {
		// big endian
		uint32_t *_4bytes = (uint32_t *)userHeader;
		for (int i = 0; i < pds->userHeaderNumber; ++i) {
			BYTESWAP(*_4bytes++);
		}
	}
	zspdi6_(ifltab, pds->pathname, &pds->numberOrdinates, &pds->numberCurves, &pds->boolIndependentIsXaxis,
		unitsIndependent, typeIndependent, unitsDependent, typeDependent,
		(void *)svalues, (void *)dvalues, &boolDouble, (const char *)clabel, &labels, userHeader, &pds->userHeaderNumber,
		&storageFlag, &status,
		strlen(pds->pathname), strlen(unitsIndependent), strlen(typeIndependent),
        strlen(unitsDependent), strlen(typeDependent), sizeof(clabel[0]));

	free(userHeader);
	if (storageFlag < 10) {
		if (svalues) {
			free(svalues);
		}
		if (dvalues) {
			free(dvalues);
		}
	}


	return status;
}



