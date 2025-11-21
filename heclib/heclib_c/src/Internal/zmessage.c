#include <stdio.h>
#include <string.h>


#include "hecdss7.h"
#include "hecdssInternal.h"
#include "zdssMessages.h"
#include "heclib7.h"
#include "heclibDate.h"


/**
*  Function:	zmessage
*				zmessageLength
*				zmessage2
*				zmessage2NT
*				zmessageNoNl
*				zmessTime
*				zmessageDebug
*
*  Use:			Private
*
*  Description:	Send a message to log file or user
*
*  Declaration: void zmessage(long long *ifltab, const char *message)
*				void zmessageLength(long long *ifltab, const char *message, size_t len)
*				void zmessage2(long long *ifltab, const char *message1, const char *message2)
*				void zmessage2NT(long long *ifltab, const char *message1, const char *message2)
*				void zmessageNoNl(long long *ifltab, const char *message)
*				void zmessTime(long long *ifltab)
*				void zmessageDebug(long long *ifltab, int functionID, const char *message1, const char *message2)
*
*
*  Parameters:	long long *ifltab:  The ifltab array
*				int functionID:  The int identifying the calling function (from struct DssFunctions)
*				const char *message:  The message to show
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
*			zmessageDebug writes a debug preamble then write the debug message to the log file.
*				Debug messages always use ANSI text, not Unicode
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

void zmessage(long long *ifltab, const char *message)
{
	size_t len;

	len = trimLength(message);
	zmessageLength(ifltab, message, len);
}



void zmessageLength(long long *ifltab, const char *message, size_t len)
{
	int ipos;
	int i;
	int length;
	char c;

	if ((int)len == 0) {
		zmessTime(ifltab);
		zmessageLen(ifltab, message, len);
	}
	else {
		ipos = 0;
		for (i=0; i<(int)len; i++) {
			c = message[i];
			if ((c == '\0') || (c == '\n') || (i == ((int)len-1))) {
				zmessTime(ifltab);
				length = i - ipos;
				if (i == ((int)len-1)) length++;
				zmessageLen(ifltab, &message[ipos], (size_t)length);
				ipos = i + 1;
			}
		}
	}
}


void zmessage2(long long *ifltab, const char *message1, const char *message2)
{
	//  Trim the second message, but not the first.... often first is
	//  "Pathname:  "  (and we need to keep the spaces)
	size_t len;

	zmessTime(ifltab);
	len = trimLength(message2);
	zmessage2Len(ifltab, message1, strlen(message1), message2, len);
}

void zmessage2NT(long long *ifltab, const char *message1, const char *message2)
{
	//  Trim the second message, but not the first.... often first is
	//  "Pathname:  "  (and we need to keep the spaces)
	size_t len;

	len = trimLength(message2);
	zmessage2Len(ifltab, message1, strlen(message1), message2, len);
}

void zmessageNoNl(long long *ifltab, const char *message)
{
	zmessageNoNlLen(ifltab, message, (int)strlen(message));
}

void zmessTime(long long *ifltab)
{
	char ctime[14];

	getCurrentTimeString(ctime, sizeof(ctime));
	ctime[12] = ' ';
	ctime[13] = '\0';
	zmessageNoNlLen(ifltab, ctime, (size_t)13);
}

void zmessageInt(long long *ifltab, const char *message, int number)
{
	char messageString[20];

	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d", number);
	zmessage2(ifltab, message, messageString);
}

void zmessageLong(long long *ifltab, const char *message, long long number)
{
	char messageString[20];

	_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %lld", number);
	zmessage2(ifltab, message, messageString);
}



