#include <string.h>
#include <stdlib.h>

#include "heclib7.h"
#include "hecdssInternal.h"
#include "zdssMessages.h"
#include "zdssKeys.h"
#include "zerrorCodes.h"

/**
*  Function:	zmessageAvailable
*				zgetMessage
*				zgetMessageAll
*				zclearMessage
*				zclearMessageAll
*				zmessageInterface
*
*  Use:			Public / Private
*
*  Description:	Get Critical Messages (not debug messages)
*				A function to both write to the log file and send messages to main program, if available
*				This is meant for Java, VB, etc. programs, where the user needs to see
*				an important message (usually an error) in a dialog or pop up dialog,
*				not just an message to the log file.
*
*  Declaration: int zmessageAvailable(long long *ifltab)  // Public
*				const char *zgetMessage(long long *ifltab)  //  Public
*				const char *zgetMessageAll(long long *ifltab)  //  Public
*				void zclearMessage(long long *ifltab)  //  Public
*				void zclearMessageAll(long long *ifltab)  //  Public
*				void zmessageInterface(long long *ifltab, const char *message, int boolContinuation)  //  Private
*
*
*  Parameters:	long long *ifltab:  The ifltab array
*				const char *message:  The message being passed to the user or log file
*				int boolContinuation:  An int to indicate if this message may have more lines
*						to add or not.  Set to "1" if complete, "0" if may have more.
*						Generally, 0 means the call is from a lower level error, 1 is the top level
*						This parameter does not prevent messages from being written, just if another
*						part of the message might be coming.
*
*  Returns:		zmessageAvailable returns the number of messages available, or 0 if none.
*				zgetMessage returns the last message as a single string with embedded "\n", terminated by "\0"
*				zgetMessageAll returns all messages, separated by "\n" and terminated by "\0".
*					There is a limit of 10 critical messages that can be stored.
*
*
*  Notes:		zclearMessage removes the last message posted (otherwise, that message can be re-retrieved.)
*				zclearMessageAll removes all messages posted.
*
*  Example:		...
*				if (zmessageAvailable(ifltab)) {
*					printf("%s\n", zgetMessage(ifltab));
*					zclearMessage(ifltab);
*				}
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/



int zmessageAvailable(long long *ifltab)
{
	return (int)ifltab[zdssKeys.kmessagesAvail];
}

int zmessageInterface(long long *ifltab, const char *message, int boolContinuation)
{
	char *cpos;
	int len;
	int length;
	int ipos;
	int i;

	//  Do we have multiple lines in this message?
	//  If so, make separate calls to save in log file

	cpos = (char *)strchr(message, '\n');
	if (cpos != NULL) {
		len =  (int)strlen(message);
		ipos = 0;
		while (ipos < len) {
			//  Look for the (next) new line
			cpos = (char *)strchr(&message[ipos], '\n');
			//  If we don't find it, just go to the end of the line
			if (!cpos){
				cpos = (char *)message;
				cpos += len;
			}
			length = (int)(cpos - &message[ipos]);
			//  Write message to log file
			zmessageLength(ifltab, &message[ipos], (size_t)length);
			//  Go to start of next line, or beyond end of message and exit
			ipos += length;
			ipos++;
		}
	}
	else {
		zmessage(ifltab, message);
	}

	//  Now copy the message into an empty message buffer
	len = (int)strlen(message) + 1;
	if (boolContinuation) {
		i = (int)ifltab[zdssKeys.kmessagesAvail];
		if (i > 0) {
			i--;
			length = zmessageAvail.messLengths[i] + len + 1;
			cpos = (char *)calloc((size_t)length, CHAR_SIZE);
			if (!cpos) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_other_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, length, 0,
										zdssErrorSeverity.MEMORY_ERROR, "", "Message string");
			}
			stringCopy(cpos, (size_t)length, zmessageAvail.messages[i], _TRUNCATE);
			stringCat(cpos, (size_t)length, "\n", _TRUNCATE);
			stringCat(cpos, (size_t)length, message, _TRUNCATE);

			free(zmessageAvail.messages[i]);
			zmessageAvail.messages[i] = cpos;
			zmessageAvail.messLengths[i] = length;
			return 0;
		}
	}

	for (i=0; i<maxMessageAvail ; i++) {
		if (zmessageAvail.messLengths[i] == 0) {
			zmessageAvail.messLengths[i] = len;
			zmessageAvail.messages[i] = (char *)malloc((size_t)len);
			if (!zmessageAvail.messages[i]) return 0;
			stringCopy(zmessageAvail.messages[i], (size_t)len, message, _TRUNCATE);
			zmessageAvail.messHandles[i] = (int)ifltab[zdssKeys.khandle];
			zmessageAvail.numberMessages++;
			ifltab[zdssKeys.kmessagesAvail] = i + 1;
			return 0;
		}
	}
	return 0;
}



const char *zgetMessage(long long *ifltab)
{
	int ipos;
	int i;

	ipos = (int)ifltab[zdssKeys.kmessagesAvail] - 1;
	if ((ipos < 0) || (ipos >= maxMessageAvail)) return "\n";
	if (zmessageAvail.messLengths[ipos] > 0) {
		return zmessageAvail.messages[ipos];
	}
	else {
		for (i=0; i<maxMessageAvail ; i++) {
			if ((zmessageAvail.messHandles[i] == zhandle(ifltab)) &&
				(zmessageAvail.messLengths[i] > 0)) {
				ifltab[zdssKeys.kmessagesAvail] = i + 1;
				return zmessageAvail.messages[i];
			}
		}
	}
	return "\0";
}

const char *zgetMessageAll(long long *ifltab)
{
	int ipos;
	int i;

	ipos = (int)ifltab[zdssKeys.kmessagesAvail] - 1;
	if ((ipos < 0) || (ipos >= maxMessageAvail)) return "\n";
	if (zmessageAvail.messLengths[ipos] > 0) {
		return zmessageAvail.messages[ipos];
	}
	else {
		for (i=0; i<maxMessageAvail ; i++) {
			if (zmessageAvail.messLengths[i] > 0) {
				ifltab[zdssKeys.kmessagesAvail] = i + 1;
				return zmessageAvail.messages[i];
			}
		}
	}
	return "\0";
}


void zclearMessage(long long *ifltab)
{
	int ipos;

	ipos = (int)ifltab[zdssKeys.kmessagesAvail] - 1;
	if ((ipos < 0) || (ipos >= maxMessageAvail)) return;
	if (zmessageAvail.messLengths[ipos] > 0) {
		free(zmessageAvail.messages[ipos]);
		zmessageAvail.messages[ipos] = 0;
		zmessageAvail.messLengths[ipos] = 0;
		zmessageAvail.messHandles[ipos] = 0;
		zmessageAvail.numberMessages--;
		ifltab[zdssKeys.kmessagesAvail] = 0;
	}
}

void zclearMessageAll(long long *ifltab)
{
	int i;

	for (i=0; i<maxMessageAvail ; i++) {
		if (zmessageAvail.messLengths[i] > 0) {
			free(zmessageAvail.messages[i]);
			zmessageAvail.messages[i] = 0;
			zmessageAvail.messLengths[i] = 0;
			zmessageAvail.messHandles[i] = 0;
		}
	}
	zmessageAvail.numberMessages = 0;
	ifltab[zdssKeys.kmessagesAvail] = 0;
}

