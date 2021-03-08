
#include <stdio.h>
#include <string.h>

#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "heclib7.h"
#include "zdssVals.h"
#include "zdssKeys.h"


/**
*  Function:	zmessageZget
*
*  Use:			Private (Internal)
*
*  Description:	 Writes debug message for zget and derivatives.
*				 This is a convenience function to keep other code from being too cluttered.
*
*  Declaration: void zmessageZget(long long *ifltab, long long iaddress, int number, int wordSize, int bufferAction,
*							   int status, int functionID, const char* additional)
*
*  Parameters:	long long *ifltab
*					The integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				long long iaddress
*					The int*8 address that is being read.
*
*				int number
*					The number that will be read.
*
*				int wordSize
*					The word size in int*4 words.  Must be either 1 or 2.
*					1 - reading int*4 words
*					2 - reading int*8 words
*
*				int bufferAction
*					A flag indicating how to handle buffering for this call:
*						BUFF_NO_ACTION – no buffer use in this io; read data directly from disk
*						BUFF_READ - Copy from buffer; if not in it, return
*						BUFF_LOAD - load buffer only - don't return values
*
*				int status
*					status of the action, if applicable
*
*				const char* functionName
*					Name of the calling function from the message header
*
*				const char* additional
*					Any additional information to add to the message
*
*
*	See Also:	zget, zgetBuff, zmessageZput
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void zmessageZget(long long *ifltab, long long iaddress, int number, int wordSize, int bufferAction,
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
			"Physical read at address     %lld; end %lld; wordSize: %d; ints %d; longs %d; handle %d; status %d.  %s",
			iaddress, (iaddress + ilongs - 1), wordSize, number, ilongs, ihandle, status, additional);
	}
	else if (bufferAction == BUFF_READ) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"Read from buffer at address  %lld; end %lld; wordSize: %d; ints %d; longs %d; handle %d; status %d.  %s",
			iaddress, (iaddress + ilongs - 1), wordSize, number, ilongs, ihandle, status, additional);
	}
	else if (bufferAction == BUFF_LOAD) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"Buffer loaded from address   %lld; end %lld; wordSize: %d; ints %d; longs %d; handle %d; status %d.  %s",
			iaddress, (iaddress + ilongs - 1), wordSize, number, ilongs, ihandle, status, additional);
	}
	else if (bufferAction == 3) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"Buffer dumped to disk at     %lld; end %lld; wordSize: %d; ints %d; longs %d; handle %d; status %d.  %s",
			iaddress, (iaddress + ilongs - 1), wordSize, number, ilongs, ihandle, status, additional);
	}
	else if (bufferAction == 4) {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"Data not in buffer; address  %lld; end %lld; wordSize: %d; ints %d; longs %d; handle %d; status %d.  %s",
			iaddress, (iaddress + ilongs - 1), wordSize, number, ilongs, ihandle, status, additional);
	}
	else {
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"Unrecognized Action: %d", bufferAction);
	}
	zmessageDebug(ifltab, functionID, messageString, "");
}

