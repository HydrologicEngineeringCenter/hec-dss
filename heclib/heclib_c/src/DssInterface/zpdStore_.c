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


int zpdstore_(long long *ifltab, const char *pathname,
					void *ordinates, void *values, int valueSize,
					int *numberOrdinates, int *numberCurves,
					const char *unitsIndependent, const char *typeIndependent,
					const char *unitsDependent, const char *typeDependent,
					const char *labels, int *boolStoreLabels,
					int *boolIndependentIsXaxis,
					int *userHeader, int *userHeaderNumber,
					const char *timeZoneName,
					int *xprecision, int *yprecision, int *storageFlag,
					slen_t pathnameLen,
					slen_t unitsIndependentLen, slen_t typeIndependentLen,
					slen_t unitsDependentLen, slen_t typeDependentLen,
					slen_t lablesLength, slen_t timeZoneNameLen)

{

	int status;

	int i;
	int ipos;
	int jpos;
	int len;
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

	if (valueSize == 1) {
		pds = zstructPdNewFloats(path, (float *)ordinates, (float *)values,
							 *numberOrdinates, *numberCurves,
							 cunitsIndependent, ctypeIndependent,
							 cunitsDependent, ctypeDependent);

	}
	else if (valueSize == 2) {
		pds = zstructPdNewDoubles(path, (double *)ordinates, (double *)values,
							 *numberOrdinates, *numberCurves,
							 cunitsIndependent, ctypeIndependent,
							 cunitsDependent, ctypeDependent);
	}
	else {
		//  Error out
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

	pds->boolIndependentIsXaxis = *boolIndependentIsXaxis;
	pds->userHeader = userHeader;
	//  FIX ME - CHANGE LENGTH TO NUMBER
	pds->userHeaderNumber = *userHeaderNumber;
	pds->xprecision = *xprecision;
	pds->yprecision = *yprecision;
	if (timeZoneName && (timeZoneNameLen > 0)) {
		pds->timeZoneName = stringFortToC(timeZoneName, timeZoneNameLen);
		pds->allocated[zSTRUCT_timeZoneName] = 1;
	}

	status = zpdStore(ifltab, pds, *storageFlag);

	if (clabels) {
		free(clabels);
		clabels = 0;
	}

	free(path);
	free(cunitsIndependent);
	free(ctypeIndependent);
	free(cunitsDependent);
	free(ctypeDependent);


	return status;
}



