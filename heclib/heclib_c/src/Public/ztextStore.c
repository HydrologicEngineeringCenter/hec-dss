#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"


/**
*  Function:	ztextStore
*
*  Use:			Public
*
*  Description:	Store a text record
*
*  Declaration: int ztextStore(long long *ifltab, zStructText *textStruct);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				zStructText *textStruct
*					A text struct contain text data to write to a single text record.
*					This struct is created by the following method:
*						zStructText* zstructTextNew(const char* pathname);
*					with pathname being an valid pathname
*					When you are finished with the struct, the struct must be freed by a call to
*						void zstructFree(zStructText *textStruct)
*					NEVER REUSE A zStructText, always free and create a new on.
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*
*	Function to use to create zStructText:
*
*				zStructText *textStructNew(const char* pathname);
*
*
*	Function to free zStructText
*				void zstructFree(zStructText *textStructNew);  //  All DSS structs are freed by this call.
*
*
*  zStructText parameters used in this call:
*
*	Required:
*
*				const char* pathname
*					The pathname of the record to store.  The pathname must be exact, but is case insensitive
*					(i.e., "Apart" is the same as "APART").  There are no specific conventions for text pathnames.
*
*	Optional:
*
*				char *textString
*				int numberTextChars
*					If you want to store a text string or array, then textString points to the char array
*					and numberTextChars is the length to store.  The array may contain nulls ('\0'), '\n'
*					and other characters.  The string(s) is not null terminated for storing; numberTextChars
*					defines the length.  You can store a text string and text table in the same record.
*					A text string can be long; for example, it may be an entire chapter from a book.
*					If you are not storing a text string, set numberTextChars to 0.
*
*				char *textTable
*				int numberTableChars
*				int numberRows
*				int numberColumns
*					If you want to store a text list or text table, then textTable points to the char array
*					and numberTableChars is the length to store, including nulls.  A text list is just a
*					text table with 1 column.  Each cell in the table must be a null terminated string.
*					There must be (numberRows * numberColumns) null terminated strings in textTable.
*					For further definition, see comments below.
*					You can store a text string and text table in the same record.
*					If you are not storing a text string, set numberTableChars to 0.
*
*				char *labels
*				int numberLabelChars
*					If the table has a labels or header row, then labels points to the char array
*					and numberLabelChars is the length to store, including nulls.
*					Each cell in the table must be a null terminated string.
*					There must be numberColumns null terminated strings in labels.
*					If you are not storing labels, set numberLabelChars to 0.
*
*
*
*
*	Comments:
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
*		(A departure from paired data conventions, the first label belongs to the independent (first) column.)
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

int ztextStore(long long *ifltab, zStructText *textStruct)
{

	int numberInts;
	int numberChars;
	int status;

	int internalHeader[INT_HEAD_SIZE];

	zStructTransfer* ztransfer;


	if (!textStruct) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextStore_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "textStruct is null");
	}
	if (!textStruct->pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextStore_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "textStruct pathname is null");
	}

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztextStore_ID, "Enter Pathname: ", textStruct->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztextStore_ID, "  Handle: ",  zhandle(ifltab));
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztextStore_ID, "  Length of text string: ",  textStruct->numberTextChars);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztextStore_ID, "  Length of text table: ",  textStruct->numberTableChars);
		if (textStruct->numberTableChars > 0) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_ztextStore_ID, "  Number Rows: ",  textStruct->numberRows);
			zmessageDebugInt(ifltab, DSS_FUNCTION_ztextStore_ID, "  Number Columns: ",  textStruct->numberColumns);
		}
	}

	//  Some basic error checking
	//  We require a positive number of characters to store
	numberChars = textStruct->numberTableChars + textStruct->numberTextChars;
	if (numberChars < 1)  {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextStore_ID, zdssErrorCodes.INVALID_NUMBER_TO_WRITE,
										 numberChars, 0, zdssErrorSeverity.WARNING, textStruct->pathname,
										 "(textStruct->numberTableChars + textStruct->numberTextChars) < 1");
	}
	if ((textStruct->numberTextChars > 0) && !textStruct->textString) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextStore_ID, zdssErrorCodes.INVALID_NUMBER_TO_WRITE,
										 textStruct->numberTextChars, 0, zdssErrorSeverity.WARNING, textStruct->pathname,
										 "textStruct->numberTextChars");
	}
	if ((textStruct->numberTableChars > 0) && !textStruct->textTable) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextStore_ID, zdssErrorCodes.INVALID_NUMBER_TO_WRITE,
										 textStruct->numberTableChars, 0, zdssErrorSeverity.WARNING, textStruct->pathname,
										 "textStruct->numberTableChars");
	}
	if ((textStruct->numberLabelChars > 0) && (!textStruct->labels)) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextStore_ID, zdssErrorCodes.INVALID_NUMBER_TO_WRITE,
										 textStruct->numberLabelChars, 0, zdssErrorSeverity.WARNING, textStruct->pathname,
										 "textStruct->numberLabelChars");
	}

	ztransfer = zstructTransferNew(textStruct->pathname, 0);
	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_ztextStore_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
								zdssErrorSeverity.MEMORY_ERROR,
								textStruct->pathname, "Allocating ztransfer struct");
	}

	if (textStruct->numberColumns > 1) {
		ztransfer->dataType = DATA_TYPE_TEXT_TABLE;
	}
	else {
		ztransfer->dataType = DATA_TYPE_TEXT;
	}

	//  We have to store characters in an int array and have to use charInt
	//  to take care of big endian, little endian issues
	if (textStruct->numberTextChars > 0) {
		numberInts = numberIntsInBytes(textStruct->numberTextChars);
		ztransfer->values1 = (int *)calloc((size_t)numberInts + 2, WORD_SIZE);
		charInt((void *)textStruct->textString, (void *)ztransfer->values1, textStruct->numberTextChars, textStruct->numberTextChars, 1, 0, 0);
		ztransfer->values1Number = numberInts;
	}
	else {
		ztransfer->values1 = 0;
		ztransfer->values1Number = 0;
	}

	if (textStruct->numberTableChars > 0) {
		numberInts = numberIntsInBytes(textStruct->numberTableChars);
		ztransfer->values2 = (int *)calloc((size_t)numberInts+2, WORD_SIZE);
		charInt((void *)textStruct->textTable, (void *)ztransfer->values2, textStruct->numberTableChars, textStruct->numberTableChars, 1, 0, 0);
		ztransfer->values2Number = numberInts;
	}
	else {
		ztransfer->values2 = 0;
		ztransfer->values2Number = 0;
	}

	//  If we have labels, store them in header2
	if (textStruct->numberLabelChars > 0) {
		numberInts = numberIntsInBytes(textStruct->numberLabelChars);
		ztransfer->header2 = (int *)calloc((size_t)numberInts+2, WORD_SIZE);
		charInt((void *)textStruct->labels, (void *)ztransfer->header2, textStruct->numberLabelChars, textStruct->numberLabelChars, 1, 0, 0);
		ztransfer->header2Number = numberInts;
	}
	else {
		ztransfer->header2 = 0;
		ztransfer->header2Number = 0;
	}

	//  Fill out internal header
	internalHeader[0] = textStruct->numberTextChars;
	internalHeader[1] = textStruct->numberTableChars;
	internalHeader[2] = textStruct->numberLabelChars;
	internalHeader[3] = textStruct->numberRows;
	internalHeader[4] = textStruct->numberColumns;
	internalHeader[5] = 0; // to hold swapped value on big endian systems

	ztransfer->internalHeader = internalHeader;
	ztransfer->internalHeaderNumber = 6;
	ztransfer->userHeader = textStruct->userHeader;
	ztransfer->userHeaderNumber = textStruct->userHeaderNumber;


	status = zwrite(ifltab, ztransfer);

	if (ztransfer->values1) {
		free(ztransfer->values1);
	}
	if (ztransfer->values2) {
		free(ztransfer->values2);
	}
	if (ztransfer->values3) {
		free(ztransfer->values3);
	}
	if (ztransfer->header2) {
		free(ztransfer->header2);
	}
	zstructFree(ztransfer);

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztextStore_ID, "Exit Pathname: ", textStruct->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztextStore_ID, "  Status: ",  status);
	}

	if (zisError(status)) {
		//  An error code
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_ztextStore_ID);
	}

	return status;
}

