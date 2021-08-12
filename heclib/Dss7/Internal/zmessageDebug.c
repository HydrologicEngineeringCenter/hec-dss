#include <stdio.h>
#include <stdlib.h>

#include <string.h>

#include "heclib7.h"
#include "hecdssInternal.h"
#include "heclibDate.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "zdssMessages.h"

/**
*  Function:	zmessageDebug
*
*  Use:			Private
*
*  Description:	Write Debug preamble then write the debug message to log file
*
*  Declaration: void zmessageDebug(long long *ifltab, int functionID, const char *message1, const char *message2)
*
*
*  Parameters:	long long *ifltab:  The ifltab array
*				int functionID:  The int identifying the calling function (from struct DssFunctions)
*				const char *message1:  First part of message
*				const char *message2:  Second part of message
*
*
*  Notes:	zmessageCombined does physical writing and is library dependent
*			(these functions are library independent)
*
*			zmessage prepends time and appends CR/LF, then writes to message/log
*			zmessageLength prepends time, uses passed length and appends CR/LF, then writes to message/log
*			zmessage2 prepends time, combines messages and appends CR/LF, then writes to message/log
*			zmessage2NT combines messages and appends CR/LF, then writes to message/log.  No time is prepended.
*			zmessageNoNl just writes to message/log.  No CR/LF is appended.
*			zmessTime writes the time to the message/log.
*
*  See Also:	zmessageCombined
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


void zmessageDebug(long long *ifltab, int functionID, const char *message1, const char *message2)
{
	//  ASCII only!  For porting installation only - does not follow Unicode standards...

	char dmess[20] = " =====DSS===Debug: ";
	char semicolon[2] = ";";
	char blank[2] = " ";
	char mess[500];

	getCurrentTimeString(mess, sizeof(mess));

	stringCat(mess, sizeof(mess), dmess, _TRUNCATE);
	if (zgetVersion(ifltab) == 6) stringCat(mess, sizeof(mess), "Version 6; ", _TRUNCATE);
	stringCat(mess, sizeof(mess), zgetFunctionName(functionID), _TRUNCATE);
	stringCat(mess, sizeof(mess), semicolon, _TRUNCATE);
	stringCat(mess, sizeof(mess), blank, _TRUNCATE);
	if (message1) {
		stringCat(mess, sizeof(mess), message1, _TRUNCATE);
	}
	else {
		stringCat(mess, sizeof(mess), "<message 1 is NULL!> ", _TRUNCATE);
}
	stringCat(mess, sizeof(mess), blank, _TRUNCATE);
	if (message2) {
		stringCat(mess, sizeof(mess), message2, _TRUNCATE);
	}
	else {
		stringCat(mess, sizeof(mess), "<message 2 is NULL!> ", _TRUNCATE);
	}
	zmessageLen(ifltab, mess, strlen(mess));

}

void zmessageDebugInt(long long *ifltab, int functionID, const char *message1, int number)
{
	char messageString[20];

	if (message1) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d", number);
		zmessageDebug(ifltab, functionID, message1, messageString);
	}
	else {
		zmessageDebug(ifltab, functionID, "<message 1 is NULL!> ", messageString);
	}
}

void zmessageDebugLong(long long *ifltab, int functionID, const char *message1, long long number)
{
	char messageString[20];

	if (message1) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %lld", number);
		zmessageDebug(ifltab, functionID, message1, messageString);
	}
	else {
		zmessageDebug(ifltab, functionID, "<message 1 is NULL!> ", messageString);
	}
}

void zmessageDebugFloat(long long* ifltab, int functionID, const char* message1, float number)
{
	char messageString[20];

	if (message1) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %f", number);
		zmessageDebug(ifltab, functionID, message1, messageString);
	}
	else {
		zmessageDebug(ifltab, functionID, "<message 1 is NULL!> ", messageString);
	}
}
