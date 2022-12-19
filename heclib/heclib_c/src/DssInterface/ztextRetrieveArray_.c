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



void ztextretrievearray_ (long long *ifltab, const char *pathname, char* clines, int *maxLines,
						  int *numberLines, int *userHeader, int *maxHeader, int *numberUserHeader,
						  int *istatus, slen_t lenPathname, slen_t lenClines)
{
	int status;
	int i;
	int max;
	int ipos;
	int len;
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

	if (textStruct->numberTextChars > 0) {
		stringCToFort(clines, lenClines, textStruct->textString);
	}
	else if (textStruct->numberTableChars > 0) {
		max = textStruct->numberRows;
		if (*maxLines < max) {
			max = *maxLines;
		}
		ipos = 0;
		*numberLines = max;
		for (i=0; i<max; i++) {
			len = (int)strlen(&textStruct->textTable[ipos]);
 			stringCToFort(&clines[i*lenClines], (size_t)lenClines, &textStruct->textTable[ipos]);
			ipos += len + 1;
			if (ipos >= textStruct->numberTableChars) {
				break;
			}
		}
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

