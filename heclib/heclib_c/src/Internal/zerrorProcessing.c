#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zerrorCodes.h"
#include "zdssVals.h"
#include "hecdssInternal.h"


/**
*  Function:	zerrorProcessing
*
*  Use:			Private (Internal)
*
*  Description:	Processing of all (significant) errors that might occur in DSS.  This may not include informational
*					messages, such as missing data.
*
*  Called By:	various functions
*
*  Declaration: int zerrorProcessing(long long *ifltab, int functionID, int errorNumber, int status,
*					 long long iaddress, int severity, const char *pathname, const char *message)
*
*  Parameters:	long long ifltab
*					The integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				int functionID
*					The function identifier of the calling function.  This is defined in header file "zdssMessages.h"
*
*				int errorNumber
*					The	DSS error number of the error that occurred.  This number is defined in file "zerrorCodes.h"
*					It must have a corresponding string defining the error, in errorMess[]
*
*				int status
*					The system or other error code that occurred, if any.  If non, set to zero
*
*				long long iaddress
*					The file address associated with this error, if any.
*
*				int severity
*					The severity of this error, as defined in "zerrorCodes.h", with 1 being informational and 9 being critical.
*
*				const char *pathname
*					If a record or dataset is associated with this error, then this is the pathname of that record.
*					If not, leave blank.  In some cases, this will be the file name, as the ifltab will not be valid
*					and the name has to be passed here.
*
*				const char *message
*					Additional information to define the error.  For example, if the error is "Invalid filename", then
*					this argument would be the filename provided.  It is not the same as the error that is generated from
*					the errorNumber or from a system error, as those are accounted for.
*
*
*	Returns:	int errorCode
*					A negative encoded (non-zero) error number that includes the original error, the DSS error,
*					the calling function and severity.  The errorCode is decoded by function zerrorDecode.
*					A positive value is not an error, but may be a return parameter (such as the number of pathnames, data, etc.)
*
*	Remarks:	When this function is called, the error message and all associated information is written
*					to the message file, regardless of message level.  If a system error is detected, that is
*					written also.  If a severe error, the error is saved to the file (if possible) and
*					the DSS error flag is raised.  You can quickly detect such an error by calling the following:
*
*					int errorSeverity = zerror();  //  The severity of any error in any DSS file / routine, or zero if no error.
*					int errorSeverity = zfileError(ifltab);  //  The severity of the error associated with this file, or zero if no error.
*					int ierror = zerrorCode(ifltab);  //  Error code associated with this file (the return from this function), or zero
*					zinquireChar(ifltab, "erro", calpha, sizeof(calpha), ierror);   //  The severity of the error, or zero for non-error
*					zquery("erro",  calpha, sizeof(calpha), ierror); //  The severity of any error in any DSS file / routine, or zero if no error.
*
*					It is wise to check for an error after a DSS function:
*						status = zwrite(ifltab, ....);
*						if ((status != STATUS_OKAY) || (zfileError(ifltab) != 0)) {
*							if (status != STATUS_OKAY) return status;
*							return zerrorCode(ifltab);
*						}
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

static hec_zdssLastError zdssLastError;


int zerrorProcessing(long long *ifltab, int functionID, int errorNumber, int status,
					 long long iaddress, int severity, const char *pathname, const char *message)
{
	int errorCode;
	int err = 0;
	char *function_name;
	char *cmess;
	char buff[MAX_LEN_ERROR_MESS];
	char buff2[MAX_LEN_ERROR_MESS];
	char errorMessage[MAX_LEN_ERROR_MESS];
	char systemErrorMessage[MAX_LEN_ERROR_MESS];
	char path[MAX_PATHNAME_SIZE];
	int pathLength;
	int boolUseErrorStruct;
	long long *info;
	long long *fileHeader;

	//  Messages are kept in a separate file for language translation

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];

	if (zmessageLevel(ifltab, MESS_METHOD_GENERAL_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_other_ID, "zerrorProcessing.  errorNumber: ", errorNumber);
		zmessageDebugInt(ifltab, DSS_FUNCTION_other_ID, "zerrorProcessing.  zdssLastError.severity: ", zdssLastError.severity);
		zmessageDebugInt(ifltab, DSS_FUNCTION_other_ID, "zerrorProcessing.  severity: ", severity);
		if (pathname) zmessageDebug(ifltab, DSS_FUNCTION_other_ID, "zerrorProcessing.  pathname: ", pathname);
		if (message) zmessageDebug(ifltab, DSS_FUNCTION_other_ID, "zerrorProcessing.  message: ", message);
	}



	//  Note:  Error processing is meant to be complete and clear, not efficient
	//
	//  Don't clear the error struct if this error is not as severe
	//  as last error (Don't replace a bad error with a warning.)
	if (severity >= zdssLastError.severity) {
		zerrorStructClear();
		boolUseErrorStruct = 1;
	}
	else {
		boolUseErrorStruct = 0;
	}


	//  Two blank lines on a significant error
	if (zmessaging.methodLevel[MESS_METHOD_GENERAL_ID] > 0 && severity > zdssErrorSeverity.WARNING) {
		zmessage(ifltab, " ");
		zmessage(ifltab, " ");
	}
	memset(errorMessage, 0, sizeof(errorMessage));
	memset(systemErrorMessage, 0, sizeof(systemErrorMessage));

#ifdef _MSC_VER
	_get_errno( &err );
	if ((err == 0) && (errorNumber == zdssErrorCodes.UNABLE_TO_ACCESS_FILE)) {
		err = status;
	}
	if (err != 0) {
		strerror_s(systemErrorMessage, MAX_LEN_ERROR_MESS, err);
	}
#endif

	//  zerrorMessage returns the error in characters.
	//  cmess points to an area within buff and cannot be freed
	cmess = zerrorMessage(buff, MAX_LEN_ERROR_MESS, severity, errorNumber, functionID);

	if (zmessageLevel(ifltab, MESS_METHOD_GENERAL_ID, MESS_LEVEL_USER_DIAG)) {
		if (cmess) zmessageDebug(ifltab, DSS_FUNCTION_other_ID, "zerrorProcessing.  Error: ", cmess);
	}

	//  Cases where info or addresses are in the error message
	if (errorNumber == zdssErrorCodes.INVALID_ADDRESS) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, iaddress);
		if (message) zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, message);
		zerrorSave(ifltab, zdssErrorCodes.INVALID_ADDRESS);
	}
	else if (errorNumber == zdssErrorCodes.INVALID_NUMBER_TO_READ) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, status);
		if (message) zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, message);
	}
	else if (errorNumber == zdssErrorCodes.READ_ERROR) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, status);
		if (message) zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, message);
	}
	else if (errorNumber == zdssErrorCodes.WRITE_ERROR) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, status);
		if (message) zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, message);
	}
	else if (errorNumber == zdssErrorCodes.CANNOT_LOCK_MULTI_USER) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, status);
		if (message) zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, message);
	}
	else if (errorNumber == zdssErrorCodes.ARRAY_TOO_SMALL) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, status, iaddress);
		if (message) zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, message);
	}
	else if (errorNumber == zdssErrorCodes.CANNOT_ALLOCATE_MEMORY) {
		//  Not a file error (hopefully!), but fatal
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, status);
		ifltab[zdssKeys.kerrorSevere] = 1;
		if (message) zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, message);
	}
	else if (errorNumber == zdssErrorCodes.READ_BEYOND_EOF) {
		status = abs(status);
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, status);
		if (message) zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, message);
	}
	else if (errorNumber == zdssErrorCodes.INVALID_FILE_HEADER) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, fileHeader[zdssFileKeys.kendFileHeader], DSS_END_HEADER_FLAG);
		if (message) zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, message);
	}
	else if (errorNumber == zdssErrorCodes.INVALID_HEADER_PARAMETER) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, iaddress);
		if (message) zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, message);
		if (pathname) zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, pathname);
	}
	else if (errorNumber == zdssErrorCodes.INVALID_DSS_FILE) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, message);
	}
	else if (errorNumber == zdssErrorCodes.TRUNCATED_FILE) {
		status = abs(status) - 10;
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, status, iaddress);
		if (message) zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, message);
	}
	else if (errorNumber == zdssErrorCodes.DAMAGED_FILE) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, status, iaddress);
		if (message) zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, message);
		ifltab[zdssKeys.kerrorSevere] = 1;
	}
	else if (errorNumber == zdssErrorCodes.IFLTAB_CORRUPT) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, status, iaddress);
		if (message) zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, message);
		ifltab[zdssKeys.kerrorSevere] = 1;
	}
	else if (errorNumber == zdssErrorCodes.BIN_SIZE_CONFLICT) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, iaddress, status, (int)fileHeader[zdssFileKeys.kbinSize], message);
		ifltab[zdssKeys.kerrorSevere] = 1;
	}
	else if (errorNumber == zdssErrorCodes.KEY_CORRUPT) {
		zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, cmess);
		cmess = zerrorMessage(buff, MAX_LEN_ERROR_MESS, severity, zdssErrorCodes.KEY_VALUE, functionID);
		if (ifltab[zdssKeys.kintegrityKey1] != zdssVals.integrityKey) {
			_snprintf_s(buff2, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, 1, ifltab[zdssKeys.kintegrityKey1], zdssVals.integrityKey);
			zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, buff2);
		}
		if (ifltab[zdssKeys.kintegrityKey2] != zdssVals.integrityKey) {
			_snprintf_s(buff2, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, 2, ifltab[zdssKeys.kintegrityKey2], zdssVals.integrityKey);
			zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, buff2);
		}
		if (ifltab[zdssKeys.kintegrityKey3] != zdssVals.integrityKey) {
			_snprintf_s(buff2, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, 2, ifltab[zdssKeys.kintegrityKey3], zdssVals.integrityKey);
			zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, buff2);
		}
		if (message) zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, message);
		zerrorSave(ifltab, zdssErrorCodes.KEY_CORRUPT);
		ifltab[zdssKeys.kerrorSevere] = 1;
	}
	else if (errorNumber == zdssErrorCodes.DIFFERENT_RECORD_TYPE) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, status, iaddress, message);
	}
	else if (errorNumber == zdssErrorCodes.DIFFERENT_PROFILE_NUMBER) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, status, iaddress, message);
	}
	else if (errorNumber == zdssErrorCodes.WRONG_RECORD_TYPE) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, status, iaddress, message);
	}
	else if (errorNumber == zdssErrorCodes.ARRAY_SPACE_EXHAUSTED) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, status, iaddress, message);
	}
	else if (errorNumber == zdssErrorCodes.INVALID_PARAMETER) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, message);
	}
	else if (errorNumber == zdssErrorCodes.INVALID_NUMBER) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, status, message);
	}
	else if (errorNumber == zdssErrorCodes.INVALID_INTERVAL) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, status, message);
	}
	else if (errorNumber == zdssErrorCodes.INCOMPATIBLE_VERSION) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, status);
		if (message) zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, message);
	}
	else if (errorNumber == zdssErrorCodes.CANNOT_SQUEEZE) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, message);
	}
	else if (errorNumber == zdssErrorCodes.NON_EMPTY_FILE) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, message, status);
	}
	else if (errorNumber == zdssErrorCodes.INVALID_BIN_STATUS) {
		_snprintf_s(errorMessage, MAX_LEN_ERROR_MESS, _TRUNCATE, cmess, message, status);
	}
	else {
		//  Just an error message
		zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, cmess);
		if (message) zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, message);
		if (pathname && (strlen(pathname) > 1)) {
			zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, " ");
			zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, pathname);
		}
	}

	if (zgetVersion(ifltab) == 6) {
		zmessage2(ifltab, cmess, "");
		zmessage2(ifltab, "Error occurred in HEC-DSS version 6, message: ", message);
		if (pathname) zmessage2(ifltab, "Pathname or File name: ", pathname);
		if (status == 0) {
			status = -errorNumber;
		}
		return status;
	}

	if (boolUseErrorStruct) {
		zdssLastError.severity = severity;
		zdssLastError.errorNumber = errorNumber;
		zdssLastError.systemError = err;
		zdssLastError.lastAddress = iaddress;
		zdssLastError.functionID = functionID;
		if ((pathname) && (strlen(pathname) > 0)) {
			stringCopy(zdssLastError.lastPathname, MAX_PATHNAME_SIZE, pathname, strlen(pathname));
		}
		stringCopy(zdssLastError.errorMessage, MAX_LEN_ERROR_MESS, errorMessage, strlen(errorMessage));
		if (err) {
			stringCopy(zdssLastError.systemErrorMessage, MAX_LEN_ERROR_MESS, systemErrorMessage, strlen(systemErrorMessage));
		}
	}


	//  Did a system error occur?
	if (err) {
		zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, systemErrorMessage);
	}

	//  If this is just a warning or informational message, do not include any address or other "error" info
	if (severity >= zdssErrorSeverity.WARNING_NO_FILE_ACCESS) {
		//  Is an address included?
		if (iaddress > 0) {
			_snprintf_s(buff, MAX_LEN_ERROR_MESS, _TRUNCATE, ERROR_AT_ADDRESS, iaddress);
			zmessConcat1(errorMessage, MAX_LEN_ERROR_MESS, buff);
		}
	}
	//  Do we have a pathname associated with this error?
	if (pathname && (strlen(pathname) > 0)) {
		zmessConcat2(errorMessage, MAX_LEN_ERROR_MESS, ERROR_PATHNAME, pathname);
		if (boolUseErrorStruct) {
			stringCopy(zdssLastError.lastPathname, MAX_PATHNAME_SIZE, pathname, strlen(pathname));
		}
	}
	else {
		info = (long long *)ifltab[zdssKeys.kinfo];
		if ((ifltab[zdssKeys.kinfo] > 0) && (info[0] == DSS_INFO_FLAG)) {
			pathLength = (int)info[zdssInfoKeys.kinfoPathnameLength];
			if ((pathLength > 4) && (pathLength < MAX_PATHNAME_LENGTH)) {
				charInt((void *)&info[zdssInfoKeys.kinfoPathname], (void *)path,  pathLength, MAX_PATHNAME_LENGTH, 0, 1, 0);
				if (path[0] == '/') {
					path[pathLength] = '\0';
					zmessConcat2(errorMessage, MAX_LEN_ERROR_MESS, ERROR_LAST_PATH, path);
					if (boolUseErrorStruct) {
						stringCopy(zdssLastError.lastPathname, MAX_PATHNAME_SIZE, path, strlen(path));
					}
				}
			}
		}

		//  Write which function the error occurred in to the log file
		function_name = zgetFunctionName(functionID);
		zmessConcat2(errorMessage, MAX_LEN_ERROR_MESS, ERROR_IN_FUNCTION, function_name);
	}

	if (ifltab[zdssKeys.kfullFilename] > 0) {
		//_snprintf_s(buff, MAX_LEN_ERROR_MESS, _TRUNCATE, "%s", (const char*)&ifltab[zdssKeys.kfilename]); MAX_FILENAME_LENGTH
		charLong((void *)ifltab[zdssKeys.kfullFilename], buff, 0, sizeof(buff), 0, 1);
		zmessConcat2(errorMessage, MAX_LEN_ERROR_MESS, ERROR_IN_FILE, buff);
		if (boolUseErrorStruct) {
			charLong((void *)ifltab[zdssKeys.kfullFilename], zdssLastError.filename, 0, sizeof(zdssLastError.filename), 0, 1);
		}
	}
	else {
		//  If an invalid ifltab, the file name is passed in as the pathname
		if ((pathname) && (strlen(pathname) > 0)) {
			stringCopy(zdssLastError.filename, MAX_FILENAME_LENGTH, pathname, strlen(pathname));
			zdssLastError.lastPathname[0] = '\0';
		}
	}


	//  Encode the error information into a single int
	errorCode = zerrorEncode(severity, 0, functionID, errorNumber, status);
	if (boolUseErrorStruct) {
		zdssLastError.errorCode = errorCode;
		if (zdssLastError.severity >= zdssErrorSeverity.MEMORY_ERROR) {
			zdssLastError.errorType = ERROR_TYPE_MEMORY;
		}
		else if (zdssLastError.severity >= zdssErrorSeverity.WRITE_ERROR) {
			zdssLastError.errorType = ERROR_TYPE_FILE;
		}
		else if (zdssLastError.severity >= zdssErrorSeverity.WARNING_NO_WRITE_ACCESS) {
			zdssLastError.errorType = ERROR_TYPE_ACCESS;
		}
		else {
			zdssLastError.errorType = ERROR_TYPE_WARNING;
		}
	}

	if (zmessageLevel(ifltab, zfunctionmap.message_group[functionID], severity)) {
		//  If an error, set global and local error flags
		if (severity >= zdssErrorSeverity.WARNING_NO_FILE_ACCESS) {
			if (severity > zdssVals.globalErrorFlag)  {
				zdssVals.globalErrorFlag = severity;
				zmessConcat1(zdssVals.globalErrorMess, MAX_LEN_ERROR_MESS, errorMessage);
			}
			if (severity > zdssKeys.kerrorCondition)  {
				ifltab[zdssKeys.kerrorCondition] = severity;
				ifltab[zdssKeys.kerrorCode] = errorCode;
			}

			if ((ifltab[zdssKeys.kerrorCondition]> 1) && (ifltab[zdssKeys.kwritingNow] != 0)) {
				//  If the file is locked, unlock it - but don't save buffers in error condition
				zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
			}
			//  Write message to both log and to program buffer
			zmessageInterface(ifltab, errorMessage, 0);
		}
		else {
			//  Write the error message to the log file
			zmessage(ifltab, errorMessage);
		}
	}

#ifdef _MSC_VER
	if (err != 0) {
		//  Clear the system error
		_set_errno( 0 );
	}
#endif

	//  Make sure the error message is flushed to disk or output
	zmessageFlush(ifltab);
	if (errorCode > 0) {
		errorCode = -errorCode;
	}
	return errorCode;
}

int zerror(hec_zdssLastError *errorStruct)
{
	errorStruct->errorCode = zdssLastError.errorCode;
	errorStruct->severity = zdssLastError.severity;
	errorStruct->errorNumber = zdssLastError.errorNumber;
	errorStruct->errorType = zdssLastError.errorType;
	errorStruct->systemError = zdssLastError.systemError;
	errorStruct->lastAddress = zdssLastError.lastAddress;
	errorStruct->functionID = zdssLastError.functionID;
	errorStruct->calledByFunction = zdssLastError.calledByFunction;
	stringCopy(errorStruct->errorMessage, MAX_LEN_ERROR_MESS, zdssLastError.errorMessage, strlen(zdssLastError.errorMessage));
	stringCopy(errorStruct->systemErrorMessage, MAX_LEN_ERROR_MESS, zdssLastError.systemErrorMessage, strlen(zdssLastError.systemErrorMessage));
	stringCopy(errorStruct->lastPathname, MAX_PATHNAME_SIZE, zdssLastError.lastPathname, strlen(zdssLastError.lastPathname));
	stringCopy(errorStruct->filename, MAX_FILENAME_LENGTH, zdssLastError.filename, strlen(zdssLastError.filename));
	return zdssLastError.severity;
}

int zerrorCheck()
{
	return zdssLastError.severity;
}

void zerrorStructClear()
{
	zdssLastError.errorCode = 0;
	zdssLastError.severity = 0;
	zdssLastError.errorNumber = 0;
	zdssLastError.errorType = 0;
	zdssLastError.systemError = 0;
	zdssLastError.lastAddress = 0;
	zdssLastError.functionID = 0;
	zdssLastError.calledByFunction = 0;
	zdssLastError.errorMessage[0] = '\0';
	zdssLastError.systemErrorMessage[0] = '\0';
	zdssLastError.lastPathname[0] = '\0';
	zdssLastError.filename[0] = '\0';

}
