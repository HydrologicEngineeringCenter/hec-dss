#include <string.h>
#include <stdlib.h>

#include "heclib.h"


/**
*  Function:	ztextRetrieve
*
*  Use:			Public
*
*  Description:	Retrieve a text record
*
*  Declaration: int ztextRetrieve(long long *ifltab, zStructText *textStruct);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				zStructText *textStruct
*					A text struct that will contain text data a single text record.
*					This struct is created by the following method:
*						zStructText* zstructTextNew(const char* pathname);
*					with pathname being an valid existing pathname
*					When you are finished with the struct, the struct must be freed by a call to
*						void zstructFree(zStructText *textStruct)
*					NEVER REUSE A zStructText, always free and create a new on.
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Comments:
*
*
*		Text and Text tables are stored using null terminated strings.  
*		An example might be color characteristics:
*
*			Color		wave length		temperature		Energy
*			Red			long			hot				low
*			Blue		short			cool		  	high
*			Yellow		med-long		warm		  	medium
*
*		In this example, the text table is stored as: 
*		�Red\0long\0hot\0low\0Blue\0short\0cool\0high\0Yellow\0med-long\0warm\0medium�
*		and the labels are "Color\0wave length\0temperature\0energy\0".
*
*		All strings (or cells) must be null terminated.  There is no limit on the number of characters
*		in a text or text table record (although one needs to be careful about exhausting memory), nor a limit on the
*		number of characters per table cell.  Table cells do not have to be the same size, only terminated with a null
*		character.  When passing in the number of characters, make sure you include the last null terminator.
*
*		Along with the table, you may include column "labels".   In this example, "Color, wave length, etc." are labels.
*
*		A text table and text string are separate parts of a record and can be stored together
*		(for example, you can have a description of the table with the table.)
*		A single column text table is called a "text list"; it is a regular text table, but with just one column.
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/



int ztextRetrieve(long long *ifltab, zStructText *textStruct)
{
	int status;
	char messageString[80];
	zStructTransfer* ztransfer;


	if (!textStruct) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextRetrieve_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "textStruct is null");
	}
	if (!textStruct->pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextRetrieve_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "textStruct pathname is null");
	}

	//  Messages and debug
	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztextRetrieve_ID, "Enter Pathname: ", textStruct->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztextRetrieve_ID, "  Handle: ",  zhandle(ifltab));
	}
	

	ztransfer = zstructTransferNew(textStruct->pathname, 1);
	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextRetrieve_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR,
								textStruct->pathname, "Allocating ztransfer struct");
	}

	status = zread(ifltab, ztransfer);
	if (zisError(status)) {
		//  An error code
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_ztextRetrieve_ID);
		zstructFree(ztransfer);
		return status;
	}
	if (status != STATUS_RECORD_FOUND) {
		if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_GENERAL))  {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZREAD_NOT_EXIST, zhandle(ifltab));
			zmessage2(ifltab, messageString, ztransfer->pathname);
		}
		zstructFree(ztransfer);
		return status;
	}
	if ((ztransfer->dataType != DATA_TYPE_TEXT) &&
		(ztransfer->dataType != DATA_TYPE_TEXT_TABLE)) {
		status = zerrorProcessing(ifltab, DSS_FUNCTION_ztextRetrieve_ID,
			zdssErrorCodes.WRONG_RECORD_TYPE, DATA_TYPE_TEXT,
			(long long)ztransfer->dataType,
			zdssErrorSeverity.WARNING, textStruct->pathname, "");
		zstructFree(ztransfer);
		return status;
	}

	if (ztransfer->internalHeaderNumber < 5) {
		status = zerrorProcessing(ifltab, DSS_FUNCTION_ztextRetrieve_ID,
			zdssErrorCodes.INVALID_RECORD_HEADER, ztransfer->dataType,
			(long long)ztransfer->internalHeaderNumber,
			zdssErrorSeverity.WARNING, textStruct->pathname,
			"Internal Header absent");
		zstructFree(ztransfer);
		return status;
	}
	if (bigEndian()) {
		zswitchInts(ztransfer->internalHeader, INT_HEAD_text_size);
	}
	//  Because someone might forget to null terminate their strings,
	//  we'll allocate one extra character and null it.
	if (ztransfer->values1Number > 0) {
		textStruct->numberTextChars = ztransfer->internalHeader[0];
		textStruct->textString = (char *)malloc((size_t)textStruct->numberTextChars+9);
		if (!textStruct->textString) {
			zstructFree(ztransfer);
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztextRetrieve_ID,
						zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, textStruct->numberTextChars, 0,
						zdssErrorSeverity.MEMORY_ERROR, textStruct->pathname, "Allocating text string");
		}
		textStruct->textString[textStruct->numberTextChars] = 0;
		charInt((void *)ztransfer->values1, (void *)textStruct->textString, textStruct->numberTextChars, textStruct->numberTextChars, 0, 0, 0);
		textStruct->allocated[zSTRUCT_TX_textString] = 1;
	}
	else {
		textStruct->textString = 0;
		textStruct->numberTextChars = 0;
	}


	if (ztransfer->values2Number > 0) {
		textStruct->numberTableChars = ztransfer->internalHeader[1];
		textStruct->textTable = (char *)malloc((size_t)textStruct->numberTableChars+9);
		if (!textStruct->textTable) {
			zstructFree(ztransfer);
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztextRetrieve_ID,
						zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, textStruct->numberTableChars, 0,
						zdssErrorSeverity.MEMORY_ERROR, textStruct->pathname, "Allocating table characters");
		}
		textStruct->textTable[textStruct->numberTableChars] = 0;
		charInt((void *)ztransfer->values2, (void *)textStruct->textTable, textStruct->numberTableChars, textStruct->numberTableChars, 0, 0, 0);
		textStruct->allocated[zSTRUCT_TX_textTable] = 1;
	}
	else {
		textStruct->numberTableChars = 0;
		textStruct->textTable = 0;
	}

	if (ztransfer->header2Number > 0) {
		textStruct->numberLabelChars = ztransfer->internalHeader[2];
		textStruct->labels = (char *)malloc((size_t)textStruct->numberLabelChars+9);
		if (!textStruct->labels) {
			zstructFree(ztransfer);
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztextRetrieve_ID,
						zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, textStruct->numberLabelChars, 0,
						zdssErrorSeverity.MEMORY_ERROR, textStruct->pathname, "Allocating labels");
		}
		textStruct->labels[textStruct->numberLabelChars] = 0;
		charInt((void *)ztransfer->header2, (void *)textStruct->labels, textStruct->numberLabelChars, textStruct->numberLabelChars, 0, 0, 0);
		textStruct->allocated[zSTRUCT_TX_labels] = 1;
	}
	else {
		textStruct->numberLabelChars = 0;
		textStruct->labels = 0;
	}

	if (ztransfer->userHeaderNumber > 0) {
		textStruct->userHeader = ztransfer->userHeader;
		textStruct->userHeaderNumber = ztransfer->userHeaderNumber;
		ztransfer->allocated[zSTRUCT_userHeader] = 0;
		textStruct->allocated[zSTRUCT_userHeader] = 1;
	}
	else {
		textStruct->userHeader = 0;
		textStruct->userHeaderNumber = 0;
	}

	textStruct->numberRows = ztransfer->internalHeader[3];
	textStruct->numberColumns = ztransfer->internalHeader[4];

	textStruct->dataType = ztransfer->dataType;
	stringCopy(textStruct->programName,  sizeof(textStruct->programName), ztransfer->programName, strlen(ztransfer->programName));
	textStruct->lastWrittenTime = ztransfer->lastWrittenTime;
	textStruct->fileLastWrittenTime = ztransfer->fileLastWrittenTime;

	zstructFree(ztransfer);

	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztextRetrieve_ID, "Exit Pathname: ", textStruct->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztextRetrieve_ID, "  Status: ",  status);
		if (status == 0) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_ztextRetrieve_ID, "  Length of text string: ",  textStruct->numberTextChars);
			zmessageDebugInt(ifltab, DSS_FUNCTION_ztextRetrieve_ID, "  Length of text table: ",  textStruct->numberTableChars);
			if (textStruct->numberTableChars > 0) {
				zmessageDebugInt(ifltab, DSS_FUNCTION_ztextRetrieve_ID, "  Number Rows: ",  textStruct->numberRows);
				zmessageDebugInt(ifltab, DSS_FUNCTION_ztextRetrieve_ID, "  Number Columns: ",  textStruct->numberColumns);
			}
		}
	}

	return status;
}


