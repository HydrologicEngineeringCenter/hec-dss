#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "fortran_string_len_size.h"


void zspdi7_(long long *ifltab, const char *pathname,
			 int *numberOrdinates, int *numberCurves, int *iHorizontal,
			 const char *unitsIndependent, const char *typeIndependent,
			 const char *unitsDependent, const char *typeDependent,
			 int *svalues, int *dvalues, int *boolDoubles,
			 const char *labels, int *boolStoreLabels,
			 int *userHeader, int *userHeaderNumber,
			 int *iplan, int *istat,
			 slen_t pathnameLen,
			 slen_t unitsIndependentLen, slen_t typeIndependentLen,
			 slen_t unitsDependentLen, slen_t typeDependentLen,
			 slen_t lablesLength)
{


	int status;

	int i;
	int ipos;
	int jpos;
	int len;
	int storageFlag;
	char *clabels;

	zStructPairedData *pds;

	char *path;
	char *cunitsIndependent;
	char *ctypeIndependent;
	char *cunitsDependent;
	char *ctypeDependent;

	path = stringFortToC(pathname, pathnameLen);
	cunitsIndependent = stringFortToC(unitsIndependent, unitsIndependentLen);
	ctypeIndependent = stringFortToC(typeIndependent, typeIndependentLen);
	cunitsDependent = stringFortToC(unitsDependent, unitsDependentLen);
	ctypeDependent = stringFortToC(typeDependent, typeDependentLen);

	if (*iplan == 10) {
		storageFlag = 10;
	}
	else {
		storageFlag = 0;
	}

	clabels = 0;


	if (*iplan == 11) {
		//  If iplan flag is 11, only one curve is sent in at a time, and
		//  the ordinates must have already been set with iplan 10
		if (*boolDoubles) {
			pds = zstructPdNewDoubles(path, (void *)dvalues, (void *)dvalues,
									  *numberOrdinates, *numberCurves,
									  cunitsIndependent, ctypeIndependent,
									  cunitsDependent, ctypeDependent);

		}
		else {
			pds = zstructPdNewFloats(path, (void *)svalues, (void *)svalues,
									 *numberOrdinates, *numberCurves,
									 cunitsIndependent, ctypeIndependent,
									 cunitsDependent, ctypeDependent);
		}
		pds->startingCurve = *numberCurves;
		pds->endingCurve = pds->startingCurve;
	}
	else {
		if (*boolDoubles) {
			ipos = *numberOrdinates * 2;
			pds = zstructPdNewDoubles(path, (void *)dvalues, (void *)&dvalues[ipos],
									  *numberOrdinates, *numberCurves,
									  cunitsIndependent, ctypeIndependent,
									  cunitsDependent, ctypeDependent);

		}
		else {
			ipos = *numberOrdinates;
			pds = zstructPdNewFloats(path, (void *)svalues, (void *)&svalues[ipos],
									 *numberOrdinates, *numberCurves,
									 cunitsIndependent, ctypeIndependent,
									 cunitsDependent, ctypeDependent);
		}

		if (*boolStoreLabels) {
			clabels = (char *) malloc((size_t)*numberCurves * (lablesLength+1));
			ipos = 0;
			for (i=0; i<*numberCurves; i++) {
				jpos = i * (int)lablesLength;
				len = copyAndTrim(&clabels[ipos], lablesLength+1, &labels[jpos], lablesLength);
				ipos += len +1;
			}
			pds->labels = clabels;
			pds->labelsLength = ipos;
		}
		else {
			clabels = 0;
		}

		pds->boolIndependentIsXaxis = *iHorizontal;
		pds->userHeader = userHeader;
		//  FIX ME - CHANGE LENGTH TO NUMBER
		pds->userHeaderNumber = *userHeaderNumber;
	}

	status = zpdStore(ifltab, pds, storageFlag);

	if (clabels) {
		free(clabels);
		clabels = 0;
	}

	free(path);
	free(cunitsIndependent);
	free(ctypeIndependent);
	free(cunitsDependent);
	free(ctypeDependent);
	zstructFree(pds);

	*istat = status;

}



