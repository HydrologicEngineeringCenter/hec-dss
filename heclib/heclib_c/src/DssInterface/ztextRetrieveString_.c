#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"




void ztextretrievestring_ (long long *ifltab, const char *pathname, char* cstring, int *maxString,
						  int *numberString, int *userHeader, int *maxHeader, int *numberUserHeader,
						  int *istatus, size_t lenPathname, size_t lenCstring)
{
	int status;
	int i;
	int max;
	int ipos;
	zStructText *textStruct;
	char *path;

	path = stringFortToC(pathname, lenPathname);
	if (!path) {
		return;
	}
	textStruct = zstructTextNew(path);
	status = ztextRetrieve(ifltab, textStruct);
	if (zisError(status)) {
		*istatus = status;
		return;
	}
	else {
		*istatus = 0;
	}

	if (textStruct->numberTextChars > 0) {
		max = textStruct->numberTextChars;
		if (*maxString < max) {
			max = *maxString;
		}
		*numberString = max;
		ipos = 0;
		for (i=0; i<max; i++) {
			cstring[i] = textStruct->textString[i];
		}
		if (max < *maxString) {
			for (i=max; i<*maxString; i++) {
				cstring[i] = ' ';
			}
		}
	}
	else if (textStruct->numberTableChars > 0) {
		max = textStruct->numberTableChars;
		if (*maxString < max) {
			max = *maxString;
		}
		ipos = 0;
		for (i=0; i<max; i++) {
			cstring[i] = textStruct->textTable[i];
		}
		if (max < *maxString) {
			for (i=max; i<*maxString; i++) {
				cstring[i] = ' ';
			}
		}
		*numberString = max;
	}

	if ((*maxHeader > 0) && (textStruct->userHeaderNumber > 0)) {
		max = textStruct->userHeaderNumber;
		if (*maxHeader < max) {
			max = *maxHeader;
		}
		for (i=0; i<max; i++) {
			userHeader[i] = textStruct->userHeader[i];
		}
		*numberUserHeader = max;
	}
	else {
		*numberUserHeader = 0;
	}

	zstructFree(textStruct);
	free(path);

}

