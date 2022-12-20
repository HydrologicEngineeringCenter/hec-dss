#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "zdssMessages.h"
#include "heclib.h"
#include "hecdssFort.h"
#include "fortran_string_len_size.h"

/**
*  Function:	ztextStoreUnit
*
*  Use:			Public
*
*  Description:	Read the contents of a Fortran file and store as text in DSS with path name pathname
*					Data is stored as a text list, where each line is a row.
*
*  Declaration: int ztextStoreFromFile(long long *ifltab, const char *pathname, const char *filename);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				const char *pathname
*					The pathname of the text record to store
*
*				int *ifortUnit
*					The unit number of the Fortran file to read from
*
*				int *numberLines  (output)
*					The number of lines read from the file
*
*				int *status  (output)
*					The status of the operation, 0 if good, < 0 if error
*
*				size_t lenPathname
*					Length of the pathname, implicitly passed by Fortran
*
*
*	See also:	ztextStoreFromFile()
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/

void ztextstoreunit_(long long *ifltab, const char *pathname, int *fortranUnit,
					 int *numberLines, int *status, slen_t lenPathname)
{

	int numberCharacters;
	int i;
	int lenString;
	int istat;
	int lineCount;
	char cline[1000];
	char *path;
	zStructText *textStruct;


	path = stringFortToC(pathname, lenPathname);
	if (path == NULL) {
		//  Memory error
		*status = zerrorProcessing(ifltab, DSS_FUNCTION_ztextStore_ID,
						zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, (int)lenPathname, 0,
						zdssErrorSeverity.MEMORY_ERROR, pathname,
						"Allocating String for pathname");
		return;
	}

	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztextStore_ID, "Enter ztextStoreUnit, Pathname: ", path);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztextStore_ID, "File unit: ", *fortranUnit);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztextStore_ID, " DSS Handle: ",  zhandle(ifltab));
	}


	textStruct = zstructTextNew(pathname);
	if (!textStruct) {
		*status = zerrorProcessing(ifltab, DSS_FUNCTION_ztextStore_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR,
								path, "Allocating textStruct");
		free(path);
		return;
	}

	//  Rewind and read file contents, putting in struct as a list (1 column table)
	//  Each line in a list is terminated with a '\0'

	lineCount = 0;
	numberCharacters = 0;
	fortranrewind_(fortranUnit);
	while (!fortranread_(fortranUnit, cline, &lenString, &istat, (sizeof(cline)-1))) {
		numberCharacters += lenString + 1;  //  1 for '\0'
		lineCount++;
	}

	//  Allocate space
	textStruct->textTable = (char *)malloc((size_t)numberCharacters);
	if (!textStruct->textTable) {
		*status = zerrorProcessing(ifltab, DSS_FUNCTION_ztextStore_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, numberCharacters, 0,
								zdssErrorSeverity.MEMORY_ERROR,
								path, "Allocating textTable");
		free(path);
		zstructFree(textStruct);
		return;
	}
	textStruct->numberRows = lineCount;
	textStruct->numberColumns = 1;
	textStruct->numberTableChars = numberCharacters;

	//  Rewind the file
	numberCharacters = 0;
	fortranrewind_(fortranUnit);
	while (!fortranread_(fortranUnit, cline, &lenString, &istat, (sizeof(cline)-1))) {
		for (i=0; i<lenString; i++) {
			textStruct->textTable[numberCharacters++] = cline[i];
		}
		textStruct->textTable[numberCharacters++] = '\0';
	}

	*status = ztextStore(ifltab, textStruct);

	*numberLines = lineCount;

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztextStore_ID, "Exit ztextStoreFromFile, Pathname: ", path);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztextStore_ID, "  Status: ",  *status);
	}

	free(path);
	zstructFree(textStruct);

	return;
}

