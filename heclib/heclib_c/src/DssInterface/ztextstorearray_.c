#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "hecdssFort.h"


/*

C
	*/


//  UNTESTED !!!!!!!!!!!!!!!!!!
//  call ztextStoreArray(ifltab, cpath, carray,  nlines, iuhead, nuhead, istat)
void ztextstorearray_ (long long *ifltab, const char *pathname, char *clines,
					 int *numberLines, int *userHeader, int *numberUserHeader,
					 int *istatus, slen_t lenPathname, slen_t lenClines)
{
	int i;
	int ipos;
	int len;
	int count;
	zStructText *textStruct;
	char *path;


	path = stringFortToC(pathname, lenPathname);
	if (!path) {
		return;
	}
	textStruct = zstructTextNew(path);

	//  Count the number of characters that we need to allocate
	count = 0;

	if (*numberLines == 1) {
		textStruct->textString = stringFortToC((const char*)clines, lenClines);
		textStruct->numberTextChars = (int)strlen(textStruct->textString);
	}
	else {
		//  Count the number of characters that we need to allocate
		count = 0;
		for (i=0; i<*numberLines; i++) {
			count += (int)trimLengthLen(&clines[i*lenClines], lenClines);
		}
		textStruct->numberTableChars = count;
		textStruct->textTable = (char *)calloc((size_t)textStruct->numberTableChars+1, CHAR_SIZE);
		if (!textStruct->textTable) {
			//  Error out
			return;
		}
		ipos = 0;
		textStruct->numberRows = *numberLines;
		for (i=0; i<*numberLines; i++) {
			len = (int)trimLengthLen(&clines[i*lenClines], lenClines);
			stringCopy(&textStruct->textTable[ipos], (size_t)(textStruct->numberTableChars - ipos), &clines[i*lenClines], (size_t)len);
			ipos += len + 1;
			if (ipos >= textStruct->numberTableChars) {
				break;
			}
		}
	}

	textStruct->userHeader = userHeader;
	textStruct->userHeaderNumber = *numberUserHeader;

	*istatus = ztextStore(ifltab, textStruct);

	if (textStruct->textString) {
		free(textStruct->textString);
		textStruct->textString = 0;
	}
	if (textStruct->textTable) {
		free(textStruct->textTable);
		textStruct->textTable = 0;
	}

	zstructFree(textStruct);
	free(path);

}

