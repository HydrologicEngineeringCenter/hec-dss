

#include "zdssVals.h"
#include "hecdss7.h"
/**
*  Function:	zopenLog
*				zcloseLog
*
*  Use:			Public
*
*  Description:	Open a text file to save DSS transactions and messages in.
*				Close the file before exiting.
*
*  Declaration: int zopenLog(const char *logFileName);
*				void zcloseLog();
*
*  Parameters:	const char *logFileName
*					The complete name of the file to write messages to
*
*
*
*	Returns:	int status
*					The system status of the file open.
*
*	Remarks:	Always call zcloseLog before exiting the program.
*					If you want to display the log file, you should close it to cause
*					messages to be flushed to it.
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/
int zopenLog(const char *logFileName)
{
	int handle;
	int status;

	if (!logFileName) {
		handle = 0;
		return zerrorProcessing((long long *)&handle, DSS_FUNCTION_zopen_ID, zdssErrorCodes.NULL_FILENAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "zopenLog");
	}

	//  Check for initialization
	 if (zdssVals.integrityKey != DSS_INTEGRITY_KEY) {
		zinit();
	 }


	status = zopenDisk(logFileName, &handle, 10, 0);
	if (status || (handle < 1)) {
		return status;
	}
	zdssVals.messageHandle = handle;
	return 0;
}

void zcloseLog()
{
	if (zdssVals.messageHandle <= 0) {
		return;
	}

	flushFile(zdssVals.messageHandle);
	closeFile(zdssVals.messageHandle);
	zdssVals.messageHandle = 0;
}

