
#include <stdio.h>
#include <string.h>

#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "heclib7.h"
#include "zdssVals.h"
#include "zdssKeys.h"


/**
*  Function:	zmessageZput
*
*  Use:			Private (Internal)
*
*  Description:	 Writes debug message for zput and derivatives.
*				 This is a convenience function to keep other code from being too cluttered.
*
*  Declaration: void zmessageZput(long long *ifltab, long long iaddress, int number, int wordSize, int bufferAction,
*							   int status, int functionID, const char* additional)
*
*  Parameters:	long long *ifltab
*					The integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				long long iaddress
*					The int*8 address that is being written.
*
*				int number
*					The number that will be written.
*
*				int wordSize
*					The word size in int*4 words.  Must be either 1 or 2.
*					1 - writing int*4 words
*					2 - writing int*8 words
*
*				int bufferAction
*					A flag indicating how to handle buffering for this call:
*						BUFF_NO_ACTION – No buffer use use; write data directly to disk
*						BUFF_WRITE - Write this data to buffer
*						BUFF_WRITE_FLUSH - Write buffer and data to disk
*
*				int status
*					status of the write action, if applicable
*
*				const char* functionName
*					Name of the calling function from the message header
*
*				const char* additional
*					Any additional information to add to the message
*
*
*	See Also:	zput, zputBuff, zmessageZget
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


void zmessageZput(long long *ifltab, long long iaddress, int number, int wordSize, int bufferAction,
			   int status, int functionID, const char* additional)
{
	int ihandle;
	long long end;
	int ilongs;
	char messageString[150];


	ihandle = (int)ifltab[zdssKeys.khandle];

	if (number > 0) {
		end = iaddress + (long long)(number/2) - 1;
	}
	else {
		end = iaddress;
	}


	ilongs = (number - 1)/2 + 1;
	if (bufferAction == BUFF_NO_ACTION) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"Write to disk at address     %lld; end %lld; wordSize: %d; ints %d; longs %d; handle %d; status %d.",
			iaddress, (iaddress + ilongs - 1), wordSize, number, ilongs, ihandle, status);
	}
	else if ((bufferAction == BUFF_WRITE) || (bufferAction == BUFF_WRITE_FLUSH)) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"Data buffered to address     %lld; end %lld; wordSize: %d; ints %d; longs %d; handle %d; status %d.",
			iaddress, (iaddress + ilongs - 1), wordSize, number, ilongs, ihandle, status);
	}
	else if (bufferAction == 3) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"Buffer dumped to disk at     %lld; end %lld; wordSize: %d; ints %d; longs %d; handle %d; status %d.",
			iaddress, (iaddress + ilongs - 1), wordSize, number, ilongs, ihandle, status);
	}
	else if (bufferAction == 4) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"Insufficient buffer size at  %lld; end %lld; wordSize: %d; ints %d; longs %d; handle %d; status %d.",
			iaddress, (iaddress + ilongs - 1), wordSize, number, ilongs, ihandle, status);
	}
	else {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"Unrecognized Action: %d", bufferAction);
	}
	zmessageDebug(ifltab, functionID, messageString, "");
	if ((int)strlen(additional) > 0) {
		zmessageDebug(ifltab, functionID, additional, "");
	}
}

