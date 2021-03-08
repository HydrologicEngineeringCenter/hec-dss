#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"


//  Fortran compatible
void zrpdi7_ (long long *ifltab, const char *pathname,
			int *numberOrdinates, int *numberCurves, int *iHorizontal,
			char *unitsIndependent, char *typeIndependent,
			char *unitsDependent, char *typeDependent,
			float *svalues, double *dvalues, int *boolDoubles,
			int *maxValues, int *numberValuesRead,
			char *labels, int *maxNumberLabels, int *boolLabelsRead,
			int *userHeader, int *userHeaderArraySize, int *numberUserHeaderRead,
			int *istat,
			size_t pathnameLen,
			size_t unitsIndependentLen, size_t typeIndependentLen,
			size_t unitsDependentLen, size_t typeDependentLen,
			size_t lablesLength)
{
	zStructPairedData *pds;



	int clabelsSize;
	int numberLabels;
	int ipos;
	int jpos;
	int len;
	int i;
	int nvalues;



	int status;

	char *path;



	path = stringFortToC(pathname, pathnameLen);
	pds = zstructPdNew(path);
	free(path);

	if (*boolDoubles) {
		status = zpdRetrieve(ifltab, pds, 2);
	}
	else {
		status = zpdRetrieve(ifltab, pds, 1);
	}
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_zpdRetrieve_ID, "zrpdi7, After paired data retrieve.  Status: ", status);
	}

	if (status == STATUS_RECORD_FOUND) {
		*istat = 0;
		*numberOrdinates = pds->numberOrdinates;
		*numberCurves = pds->numberCurves;
		stringCToFort(unitsIndependent, unitsIndependentLen,  pds->unitsIndependent);
		stringCToFort(typeIndependent, typeIndependentLen,  pds->typeIndependent);
		stringCToFort(unitsDependent, unitsDependentLen,  pds->unitsDependent);
		stringCToFort(typeDependent, typeDependentLen,  pds->typeDependent);

		if (pds->boolIndependentIsXaxis == 1) {
			*iHorizontal = 1;
		}
		else {
			*iHorizontal = 2;
		}

		if (zmessageLevel((long long*)ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_zpdRetrieve_ID, "zrpdi7, numberOrdinates: ", pds->numberOrdinates);
			zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_zpdRetrieve_ID, "zrpdi7, numberCurves: ", pds->numberCurves);
		}

		if (*boolDoubles) {
			nvalues = pds->numberOrdinates;
			if (nvalues > pds->numberOrdinates) nvalues = pds->numberOrdinates;
			convertDataArray((void *)pds->doubleOrdinates, (void *)dvalues, nvalues, 2, 2);
			ipos = nvalues;
			nvalues = pds->numberOrdinates * pds->numberCurves;
			if ((nvalues + ipos) > *maxValues) {
				if (zmessageLevel((long long*)ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
					zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_zpdRetrieve_ID, "zrpdi7, number read exceeds max values.  Number read: ", (nvalues + ipos));
					zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_zpdRetrieve_ID, "zrpdi7, maxValues: ", *maxValues);
				}
				nvalues = *maxValues - ipos;
				*istat = 1;
			}
			if (nvalues > 0) {
				convertDataArray((void *)pds->doubleValues, (void *)&dvalues[ipos], nvalues, 2, 2);
			}
			*numberValuesRead = nvalues + ipos;
		}
		else {
			nvalues = pds->numberOrdinates;
			if (nvalues > pds->numberOrdinates) nvalues = pds->numberOrdinates;
			convertDataArray((void *)pds->floatOrdinates, (void *)svalues, nvalues, 1, 1);
			ipos = nvalues;
			nvalues = pds->numberOrdinates * pds->numberCurves;
			if ((nvalues + ipos) > *maxValues) {
				if (zmessageLevel((long long*)ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
					zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_zpdRetrieve_ID, "zrpdi7, number read exceeds max values.  Number read: ", (nvalues + ipos));
					zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_zpdRetrieve_ID, "zrpdi7, maxValues: ", *maxValues);
				}
				nvalues = *maxValues - ipos;
				*istat = 1;
			}
			if (nvalues > 0) {
				convertDataArray((void *)pds->floatValues, (void *)&svalues[ipos], nvalues, 1, 1);
			}
			*numberValuesRead = nvalues + ipos;
		}

		//  Now copy the lables into the fortran character array
		if (pds->labels && labels && (*maxNumberLabels > 0)) {
			*boolLabelsRead = 1;
			ipos = 0;
			numberLabels = *numberCurves;
			clabelsSize = numberLabels * *maxNumberLabels;
			for (i=0; i<numberLabels; i++) {
				len = (int)strlen(&pds->labels[ipos]);
				jpos = i * (int)lablesLength;
				stringCToFort(&labels[jpos], (size_t)lablesLength,  &pds->labels[ipos]);
				//stringCToFort(&xlables[jpos], lablesLength,  &pds->labels[ipos]);
				ipos += len +1;
				if (ipos >= clabelsSize) {
					break;
				}
			}
		}
		else {
			*boolLabelsRead = 0;
		}

		if ((*userHeaderArraySize > 0) && (pds->userHeaderNumber > 0)) {
			len = pds->userHeaderNumber;
			if (*userHeaderArraySize < len) len = *userHeaderArraySize;
			for (i=0; i<len; i++) {
				userHeader[i] = pds->userHeader[i];
			}
			*numberUserHeaderRead = len;
		}
		else {
			*numberUserHeaderRead = 0;
		}
	}
	else if (status == STATUS_RECORD_NOT_FOUND) {
		*istat = -1;
	}
	else if (pds->dataType == 0) {
		*istat = 20;
	}
	else {
		*istat = status;
	}
	zstructFree(pds);
	if (zmessageLevel((long long*)ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt((long long*)ifltab, DSS_FUNCTION_zpdRetrieve_ID, "Exit zrpdi7,   Status: ", *istat);
	}
}

