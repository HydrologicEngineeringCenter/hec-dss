#include <stdio.h>


#include "hecdssInternal.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "heclib.h"
#include "hecdssFort.h"


/**
*  Function:	zmessageLen
*				zmessageNoNlLen
*				zmessage2Len
*				zmessageFlush
*
*  Use:			Private
*
*  Description:	Write a message to message or log file for C and/or Fortran
*
*  Declaration: void zmessageLen(long long *ifltab, const char *message, size_t length)
*				void zmessageNoNlLen(long long *ifltab, const char *message, size_t length)
*				void zmessage2Len(long long *ifltab, const char *message1, size_t len1, const char *message2, size_t len2)
*				void zmessageFlush(long long *ifltab)
*
*
*  Parameters:	long long *ifltab:  The ifltab array
*				const char *message:  The message to show
*				size_t length:  The length (number of characters) in message to write
*				const char *message1:  First part of message
*				const char *message2:  Second part of message
*				size_t len1, len2:  Lengths of messages to write
*
*
*  Notes:	These function preform physical writing and are library dependent
*
*			zmessageLen writes to message/log appending New Line character
*			zmessageNoNlLen writes to message/log without New Line character
*			zmessage2Len combines messages and appends CR/LF, then writes to message/log.
*			zmessageFlush flushes messages to disk.  Note, this is resource intensive and
*						  should only be called when necessary.
*
*  See Also:	zmessage
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

// Private
int zmessHandle(long long *ifltab, int *messHandle, int *fortMessUnit);


//  Message wtih new line
void zmessageLen(long long *ifltab, const char *message, size_t length)
{
	int handle;
	int fortUnit;
	int status;
	int one;

//	printf("%s\n", message);
	zmessHandle(ifltab, &handle, &fortUnit);

	if (fortUnit > 0) {
		one = 1;
		status = fortranwritelc_(&fortUnit, message, &one, length);
	}

	if (handle > 0) {
		writeBytes(handle, message, (unsigned int)length);
		writeBytes(handle, "\n", (unsigned int)1);
	}

	if ((handle <= 0) && (fortUnit <= 0)) {
		fwrite(message, sizeof(char), length, stdout);
		fwrite("\n", (size_t)1, (size_t)1, stdout);
	}

}

//  Message wtih no new line
void zmessageNoNlLen(long long *ifltab, const char *message, size_t length)
{
	int handle;
	int fortUnit;
	int status;
	int zero;

	//	printf("%s\n", message);
	zmessHandle(ifltab, &handle, &fortUnit);

	if (fortUnit > 0) {
		zero = 0;
		status = fortranwritelc_(&fortUnit, message, &zero, length);
	}

	if (handle > 0) {
		writeBytes(handle, message, (unsigned int)length);
	}

	if ((handle <= 0) && (fortUnit <= 0)) {
		fwrite(message, sizeof(char), length, stdout);
	}

}



void zmessage2Len(long long *ifltab, const char *message1, size_t len1, const char *message2, size_t len2)
{
	int fortUnit;
	int handle;
	int status;
	int boolval;

	zmessHandle(ifltab, &handle, &fortUnit);

	if (fortUnit > 0) {
		boolval = 0;
		status = fortranwritelc_(&fortUnit, message1, &boolval, len1);
		boolval = 1;
		status = fortranwritelc_(&fortUnit, message2, &boolval, len2);
	}
	else {
		zmessageNoNlLen(ifltab, message1, len1);
		zmessageLen(ifltab, message2, len2);
	}
}

void zmessageFlush(long long *ifltab)
{
	//  This makes sure that messages are flushed to output
	int handle;
	int fortUnit;

	zmessHandle(ifltab, &handle, &fortUnit);

	if (fortUnit > 0) {
		flush_(&fortUnit);
	}

	if (handle > 0){
		flushFile(handle);
	}
}


//  A PRIVATE function to get the C handle and Fortran unit for messages / logs
//  Returns STATUS_OKAY if defined
//  Returns STATUS_NOT_OKAY and zeros for handle / unit if not defined
//  There are times when error messages need to be shown when ifltab is not used
//  or is not properly filled out

int zmessHandle(long long *ifltab, int *messHandle, int *fortMessUnit)
{
	//  boolMessSet is informational only;
	int boolMessSet;
	char cval[5];
	int inumb;


	//  Check for initialization
	if (zdssVals.integrityKey != DSS_INTEGRITY_KEY) {
		zinit();
	}

	//  Is this a version 7 DSS file, and has ifltab been filled out
	//  It is NOT an error here if ifltab[zdssKeys.kintegrityKey1] != zdssVals.integrityKey
	//  For some error messages, ifltab will not be filled out

	if ((zgetVersion(ifltab) == 7) && (ifltab[zdssKeys.kintegrityKey1] == zdssVals.integrityKey)) {
		//  Valid ifltab
		*messHandle = (int)ifltab[zdssKeys.kmessHandle];
		*fortMessUnit = (int)ifltab[zdssKeys.kfortMessUnit];
		boolMessSet = STATUS_OKAY;
	}
	else if (zgetVersion(ifltab) == 6) {
		cval[0] = '\0';
		zquery6_("MUNIT", cval, &inumb, (size_t)4, (size_t)0);
		if (inumb > 0) {
			*fortMessUnit = inumb;
		}
		else {
			*fortMessUnit = zdssVals.fortranMessageUnit;
		}
		*messHandle = zdssVals.messageHandle;
	}
	else {
		//  ifltab not initialized (okay)
		*messHandle = zdssVals.messageHandle;
		*fortMessUnit = zdssVals.fortranMessageUnit;
		boolMessSet = STATUS_NOT_OKAY;
	}

	//  Check if the fortran message unit has been set (will be -1, if not)
	if ((*fortMessUnit < 0) || (*fortMessUnit > 2000)) {
		*fortMessUnit = zdssVals.fortranMessageUnit;
	}

	//  Do we have a valid fortan unit?
	if ((*fortMessUnit < 0) || (*fortMessUnit > 2000)) {
		//  No (but might be okay)
		*fortMessUnit = 0;
		boolMessSet = STATUS_NOT_OKAY;  //  (Not set)
	}

	//  Check if the C message handle has been set
	if ((*messHandle <= 0) || (*messHandle > 2000)) {
		*messHandle = zdssVals.messageHandle;
	}

	//  Do we have a valid handle?
	if ((*messHandle <= 0) || (*messHandle > 2000)) {
		//  No
		*messHandle = 0;
		boolMessSet = STATUS_NOT_OKAY;
	}
	return boolMessSet;
}


