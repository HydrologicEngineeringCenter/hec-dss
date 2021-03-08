#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "zdssMessages.h"
#include "heclib.h"


/**
*  Function:	ztextStoreFromFile
*
*  Use:			Public
*
*  Description:	Read the contents of a file and store as text in DSS with path name pathname
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
*				const char *filename
*					The full name of the file to read the text from.
*
*
*	See also:	ztextRetrieveToFile()
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*/


int ztextStoreFromFile(long long *ifltab, const char *pathname, const char *filename)
{

	int numberLines;
	int numberCharacters;
	int status;
	int len;
	int i;
	char cline[1000];
	zStructText *textStruct;
	FILE *textFileFP;


	if (!pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextStore_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
	}
	if (!filename) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextStore_ID, zdssErrorCodes.NULL_FILENAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
	}

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztextStore_ID, "Enter ztextStoreFromFile, Pathname: ", pathname);
		zmessageDebug(ifltab, DSS_FUNCTION_ztextStore_ID, "File name: ", filename);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztextStore_ID, " DSS Handle: ",  zhandle(ifltab));
	}

	textStruct = zstructTextNew(pathname);
	if (!textStruct) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextStore_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR,
								pathname, "Allocating textStruct");
	}

	//  Open the file and count the number of lines and characters

	//  Rewind and read file contents, putting in struct as a list (1 column table)
	//  Each line in a list is terminated with a '\0'
#ifdef _MSC_VER
	status = fopen_s(&textFileFP, filename, "r");
#else
	textFileFP = fopen(filename, "r");
	if (textFileFP) {
		status = 0;
	}
	else {
		status = -1;
	}
#endif
	if (status != 0) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextStore_ID,
					zdssErrorCodes.UNABLE_TO_ACCESS_FILE, status, 0,
					zdssErrorSeverity.WARNING, pathname, filename);
	}
	numberLines = 0;
	numberCharacters = 0;
	while (fgets(cline, sizeof(cline), textFileFP)) {
		numberCharacters += (int)strlen(cline) + 1;  //  1 for '\0'
		numberLines++;
	}

	//  Allocate space
	textStruct->textTable = (char *)malloc((size_t)numberCharacters);
	if (!textStruct->textTable) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextStore_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, numberCharacters, 0,
								zdssErrorSeverity.MEMORY_ERROR,
								pathname, "Allocating textTable");
	}
	textStruct->numberRows = numberLines;
	textStruct->numberColumns = 1;
	textStruct->numberTableChars = numberCharacters;

	//  Rewind the file
	numberCharacters = 0;
	fseek(textFileFP, (long)0, 0);
	while (fgets(cline, sizeof(cline), textFileFP)) {
		len = (int)strlen(cline);
		for (i=0; i<len; i++) {
			textStruct->textTable[numberCharacters++] = cline[i];
		}
		textStruct->textTable[numberCharacters++] = '\0';
	}
	fclose(textFileFP);

	status = ztextStore(ifltab, textStruct);
	zstructFree(textStruct);

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztextStore_ID, "Exit ztextStoreFromFile, Pathname: ", pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztextStore_ID, "  Status: ",  status);
	}

	if (zisError(status)) {
		//  An error code
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_ztextStore_ID);
	}

	return status;
}

