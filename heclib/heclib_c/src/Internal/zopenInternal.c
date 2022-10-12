
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hecdss7.h"
#include "heclib7.h"
#include "hecdssInternal.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "zerrorCodes.h"


/**
*  Function:	zopenInternal
*
*  Use:			Private (Internal)
*
*  Description:	Opens an existing or new (creates) DSS file.
*
*  Called By:	zopen, zopenExtended
*
*  Declaration: zopenInternal(long long *ifltab, const char *dssFilename, int access,
*							  int maxExpectedPathnames, int hashSize, int binSize)
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				const char *dssFilename
*					The name of the HEC-DSS file to open.  The file name may be relative or absolute.
*					The file may exist or will be created if it does not.
*					If the file extension is not ".dss", that extension will be added to the file name (for ASCII).
*					This argument may be UTF-8 (null terminated.)
*
*				int access:  read/write access to the file
*					0 - GENERAL_ACCESS:  Doesn't matter (no error if file doesn't have write permission)
*					1 - READ_ACCESS:  Read only (will not allow writing to file)
*					2 - MULTI_USER_ACCESS:  Read/Write permission with full mutil-user access
*						(usually slow, but necessary for multiple processes)
*					3 - SINGLE_USER_ADVISORY_ACCESS:  Read/Write permission with mutil-user advisory access
*						(throws an error if file is read only).  Best (and default access)
*					4 - EXCLUSIVE_ACCESS:  Exclusive write (used for squeezing).  Throws an error if not available.
*
*				int maxExpectedPathnames;  Optional - normally set to zero.
*					The maximum number of expected pathnames, used to determine the allocation size of internal tables.
*
*				int hashSize;  Optional - Recommend set to zero.
*					The size of the hash table (number of hash entries).
*					Optimal size will be:
*						maxExpectedPathnames = hashSize * binSize;
*					Generally, hashSize is about 20,000 for 1,000,000 pathnames.
*					Setting this parameter too large will waste (a significant amount of) space.
*					Too small will cause an increase in execution time.
*					(Note:  Too large will also incur increased execution time.  Optimum will be the least execution time.)
*					The minimum is one, the maximum is 100000.
*
*				int binSize;  Optional - Recommend set to zero.
*					The size of the pathname bin, in int*8 words.
*					Each pathname takes 6 + ((pathname Length-1)/8) + 1 int*8 words.
*					For an average pathname length of 60 that's 6 + 8 = 14 words.
*					A bin of 200 would hold about 200/14 = 14 pathnames.
*					If the table hash size is 8192, that means a bin for each table hash, or 8192 bins:  8192 * 14 = 114,688 pathnames.
*					If the hash codes were perfectly distributed, then the perfect number of paths would be 114,688 in the file.
*					The minimum is 30, the maximum is 400.
*
*				int boolReopen
*					A flag that indicates if standard messages should be displayed.  This is used when the file
*					is closed and then reopened to try and reduce the chance that it will go into multi-user access mode.
*
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for invalid operations detected by zopen.
*					error number returned by system open function (e.g., _sopen_s for Windows)
*
*	Note:		Function may be referred to "zopen" within code.
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


 int zopenInternal(long long *ifltab, const char *dssFilename, int access, int maxExpectedPathnames,
	 int hashSize, int binSize, int boolReopen)
 {
	 int ihandle;
	 int status;
	 int status2;
	 int fileExists;
	 int permission;
	 int size;
	 char fullDssFilename[_MAX_PATH];
	 char filenameOnly[_MAX_FNAME];
	 char *filename;
	 char fileVersion[9];
	 char cdss[9];
	 char cvers[9];
	 char messageString[200];
	 long long iarray[10];
	 long long nada=0;
	 long long dummy = 0;
	 long long *fileHeader;
	 int iswap;

	 fullDssFilename[0] ='\0';
	 
	 //  Initialize ifltab
	 status = zinitIfltab(ifltab);
	 if (zisError(status)) {
		 return status;
	 }

	 //  Has an access been set prior?
	 if (access == 0) {
		 if ((ifltab[zdssKeys.kmultiUserAccess] > 0) && (ifltab[zdssKeys.kmultiUserAccess] < 6)) {
			 access = (int)ifltab[zdssKeys.kmultiUserAccess];
		 }
	 }

	 iswap = bigEndian();
	 if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		 //  boolReopen is so open messeages are not displayed.  They are in diagnostic mode.
		 boolReopen = 0;
		zmessageDebug(ifltab, DSS_FUNCTION_zopen_ID, "Entering for file: ", dssFilename);
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE,
			"access: %d,  maxExpectedPathnames: %d,  hashSize: %d,  binSize: %d",
			access, maxExpectedPathnames, hashSize, binSize);
		zmessageDebug(ifltab, DSS_FUNCTION_zopen_ID, messageString, "");
		zmessageDebugInt(ifltab, DSS_FUNCTION_zopen_ID, "Big Endian set to ", iswap);
	 }

	 //  Get the file's absolute path, append ".dss", if needed, and determine if it exists
	 fileExists = zfileName (fullDssFilename, _MAX_PATH, dssFilename, &permission);
	 if (fileExists < 0) {
		 //  The only error is that the filename and path is not valid.
		 //  The error is system dependent
		 return zerrorProcessing(ifltab, DSS_FUNCTION_zopen_ID, zdssErrorCodes.INVALID_FILE_NAME,
								0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", dssFilename);
	 }
	 if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		if (fileExists) {
			zmessageDebug(ifltab, DSS_FUNCTION_zopen_ID, "File exists; filename: ", fullDssFilename);
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_zopen_ID, "File does not exist; filename: ", fullDssFilename);
		}
	 }
	 if (permission == 2) {
		 //  No permission access to file
		 return zerrorProcessing(ifltab, DSS_FUNCTION_zopen_ID, zdssErrorCodes.NO_PERMISSION,
								 0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", fullDssFilename);
	 }

	 if (!fileExists && (access == READ_ACCESS)) {
		 //  Read only requires file to exist
		 return zerrorProcessing(ifltab, DSS_FUNCTION_zopen_ID, zdssErrorCodes.NO_WRITE_PERMISSION,
								 0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", fullDssFilename);
	 }

	 //  We may not have enough room to store the path and filename, so store just the name.
	 filename = getFileFromPath (filenameOnly, sizeof(filenameOnly), fullDssFilename);
	 if (filename == NULL) {
		 return zerrorProcessing(ifltab, DSS_FUNCTION_zopen_ID, zdssErrorCodes.INVALID_FILE_NAME,
								 0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", fullDssFilename);
	 }

	 ifltab[zdssKeys.kopenMode] = access;

	 //  Be sure that we can access the file in the mode requested
	 if (access != READ_ACCESS) {
		 if (permission == 1) {
			 //  Read access only
			 access = READ_ACCESS;
			 if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_TERSE)) {
				 zmessage2(ifltab, "DSS file has read-only access:  ", fullDssFilename);
			 }
		 }
	 }

	 //  Exclusive access?  (for squeezing)
	 if (access == EXCLUSIVE_ACCESS) {
		status = zopenDisk(fullDssFilename, &ihandle, 10, 1);
		if (status || (ihandle < 1)) {
			//  Cannot get exclusive access
			return zerrorProcessing(ifltab, DSS_FUNCTION_zopen_ID, zdssErrorCodes.NO_EXCLUSIVE_ACCESS,
									status, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "", fullDssFilename);
		}
		ifltab[zdssKeys.kopenStatus] = OPEN_STAT_WRITE;
	 }
	 else if (access == READ_ACCESS){
		status = zopenDisk(fullDssFilename, &ihandle, 0, 0);
		if (status || (ihandle < 1)) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zopen_ID, zdssErrorCodes.UNABLE_TO_ACCESS_FILE,
									status, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "", fullDssFilename);
		}
		ifltab[zdssKeys.kopenStatus] = OPEN_STAT_READ_ONLY;
	}
	else if (fileExists) {  //  Normal access (without creating file)
		status = zopenDisk(fullDssFilename, &ihandle, 2, 0);
		if (status || (ihandle < 1)) {
			//  If the file was just deleted, it may not have been removed from
			//  Windows cache - retry
			fileExists = zfileName (fullDssFilename, _MAX_PATH, dssFilename, &permission);
			if (!fileExists) {
				status2 = zopenDisk(fullDssFilename, &ihandle, 10, 0);
			}
			//   The real error is in status
			if (status2 == 0) status = status2;
		}
		if (status || (ihandle < 1)) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zopen_ID, zdssErrorCodes.UNABLE_TO_ACCESS_FILE,
									status, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "", fullDssFilename);
		}
		ifltab[zdssKeys.kopenStatus] = OPEN_STAT_WRITE;
	}
	else {  //  Normal access (creating file)
		status = zopenDisk(fullDssFilename, &ihandle, 10, 0);
		if (status || (ihandle < 1)) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_zopen_ID, zdssErrorCodes.UNABLE_TO_CREATE_FILE,
									status, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "", fullDssFilename);
		}
		ifltab[zdssKeys.kopenStatus] = OPEN_STAT_WRITE;
	}


	 if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		 _snprintf_s(messageString, sizeof(messageString), _TRUNCATE, 	"%d;  Handle: %d", 	status, ihandle);
		 zmessageDebug(ifltab, DSS_FUNCTION_zopen_ID, "Open status: ", messageString);
	 }

	ifltab[zdssKeys.khandle] = (long long)ihandle;
	if (fileExists) {
		//  Be sure that this is a valid DSS file
		status = zreadDisk (ihandle, iswap, 0L, iarray, 8);
		if (zisError(status)) {
			// Hmmm, could be an empty file, or this is the first error to encounter
			//  If zero or only a few bytes were read, then set the file to non-existent (it's an empty file!)
			if ((status <= -10) && (status > -110)) {
				//  Less than 100 bytes in the file, we'll just consider it to be an empty file
				//  ( status < -10 is abs(bytes read) - 10
				fileExists = 0;
			}
			//  Otherwise, let the error process occur in zpermRead
		}
		else {  //   status == 0
			//  Be sure it is a DSS file!
			charLong((void *)iarray, (void *)cdss, 4, sizeof(cdss), 0, 1);
			charLong((void *)&iarray[2], (void *)cvers, 4, sizeof(cvers), 0, 1);
			//  No ZDSS header (first 4 bytes)?
			if (strncmp(cdss, zdssVals.czdss, 4) != 0) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_zopen_ID, zdssErrorCodes.INVALID_DSS_FILE,
										 0, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, fullDssFilename, cdss);
			}			
			else if (strncmp(cvers, zdssVals.czVersion, 1) != 0) {
				//  Wrong version (e.g., version 6)
				if (strncmp(cvers, "6", 1) == 0) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zopen_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
						6, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, fullDssFilename, cvers);
				}
				else if (strncmp(cvers, "8", 1) == 0) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zopen_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
						8, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, fullDssFilename, cvers);
				}
				else {
					return zerrorProcessing(ifltab, DSS_FUNCTION_zopen_ID, zdssErrorCodes.INVALID_HEADER_PARAMETER, 0,
						0, zdssErrorSeverity.CORRUPT_FILE, cvers, "Invalid DSS file version string");
				}
			}
			//  Good to go.
			//  Get the size of the permanent section
			ifltab[zdssKeys.kfiHeadSize] = iarray[1];
		}
	}

	if (fileExists) {
		//  If the file exists, load the permanent header into ifltab
		status = zpermRead(ifltab);
		if (status <= -10) {
			if (access == READ_ACCESS) {
				//  If the file exists, but it is not a valid DSS file, and
				//  we do not have write access, error out
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zopen_ID);
			}
			else {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_zopen_ID);
			}
		}
		else if (zisError(status)) {
			//  Close the file
			closeFile(ihandle);
			/////   fix me -wrong error
			////  Make status == -2 for not a dss file, but has size
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zopen_ID);
		}
		else {  //  if (status == STATUS_OKAY) {
			//  Be sure the file header was loaded...
			fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
			charLong((void *)&fileHeader[zdssFileKeys.kdss], (void *)cdss, 4, sizeof(cdss), 0, 1);
			charLong((void *)&fileHeader[zdssFileKeys.kversion], (void *)cvers, 4, sizeof(cvers), 0, 1);
			if (strncmp(cdss, zdssVals.czdss, 4) != 0) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_zopen_ID, zdssErrorCodes.INVALID_DSS_FILE,
										 0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", cdss);
			}
			else if (strncmp(cvers, zdssVals.czVersion, 1) != 0) {
				//  Wrong version (e.g., version 6)
				return zerrorProcessing(ifltab, DSS_FUNCTION_zopen_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
										 0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", cvers);
			}
			//  Save additional lock words
			fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
			ifltab[zdssKeys.klockExclusiveWord] = fileHeader[zdssFileKeys.klockAddressWord] + 1;
		}
	}
	if (fileExists) {
		fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
		charLong((void *)&fileHeader[zdssFileKeys.kversion], fileVersion, 4, sizeof(fileVersion), 0, 1);
		if (boolReopen) {
			if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_USER_DIAG)) {
				zmessage2(ifltab, ZOPEN_REOPEN_MESS, fullDssFilename);
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZOPEN_HANDLE_MESS,
					zhandle(ifltab), zdssVals.pid, zdssVals.czVersion, fileVersion);
				zmessage(ifltab, messageString);
			}
		}
		else {
			if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_TERSE)) {
				zmessage2(ifltab, ZOPEN_EXISTING_MESS, fullDssFilename);
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZOPEN_HANDLE_MESS,
					zhandle(ifltab), zdssVals.pid, zdssVals.czVersion, fileVersion);
				zmessage(ifltab, messageString);
			}
		}
	}
	else {
		if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zopen_ID, "Creating new DSS file,  File:  ", fullDssFilename);
		}
		status = zpermCreate(ifltab, maxExpectedPathnames, hashSize, binSize);
		if (status == STATUS_OKAY) {
			if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				zmessageDebug(ifltab, DSS_FUNCTION_zopen_ID, "Successfully created file", "");
			}
			if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_TERSE)) {
				zmessage2(ifltab, ZOPEN_NEW_MESS, fullDssFilename);
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZOPEN_NEW_HANDLE_MESS,
					zhandle(ifltab), zdssVals.pid, zdssVals.czVersion);
				zmessage(ifltab, messageString);
			}
		}
		else {
			//  Close the file
			closeFile(ihandle);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zopen_ID);
		}
	}

	if (access == MULTI_USER_ACCESS) {
		ifltab[zdssKeys.kmultiUserAccess] = MULTI_USER_ACCESS;
		zlockPassive(ifltab, LOCKING_LOCK_ON, LOCKING_ACCESS_READ);
	}
	//  If we are opening exclusively, lock the file,
	else if (access == EXCLUSIVE_ACCESS) {
		ifltab[zdssKeys.kmultiUserAccess] = EXCLUSIVE_ACCESS;
		status = zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_ON, LOCKING_FLUSH_OFF);
		if (zisError(status)) {
			free((long long*)ifltab[zdssKeys.kfileHeader]);
			free((long long*)ifltab[zdssKeys.kpathBin]);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zopen_ID);
		}
	}
	else {
		zlockPassive(ifltab, LOCKING_LOCK_ON, LOCKING_ACCESS_READ);
	}

	//  +9 makes sure names have enough space to swap on big endians
	size = (int)strlen(filename) + 9;
	ifltab[zdssKeys.kfilename] = (long long)malloc(size);
	charLong(filename, (void *)ifltab[zdssKeys.kfilename], 0, size, 1, 1);
	size = (int)strlen(fullDssFilename) + 9;
	ifltab[zdssKeys.kfullFilename] = (long long)malloc(size);
	charLong(fullDssFilename, (void *)ifltab[zdssKeys.kfullFilename], 0, size, 1, 1);

	if (bigEndian()) {
		ifltab[zdssFileKeys.kendian] = 1;
	}

	//  Now write the access level
	if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_TERSE) && !boolReopen) {
		if (ifltab[zdssKeys.kopenStatus] == OPEN_STAT_READ_ONLY) {
			zmessage(ifltab, ACCESS_READ_MESS);
		}
		else if (ifltab[zdssKeys.kmultiUserAccess] == MULTI_USER_ACCESS) {
			zmessage(ifltab, ACCESS_MULTI_USER_MESS);
		}
		else if (ifltab[zdssKeys.kmultiUserAccess] == SINGLE_USER_ADVISORY_ACCESS) {
			zmessage(ifltab, ACCESS_ADVISORY_MESS);
		}
		else if (ifltab[zdssKeys.kmultiUserAccess] == EXCLUSIVE_ACCESS) {
			zmessage(ifltab, ACCESS_EXCLUSIVE_MESS);
		}
	}

	//  If no access mode was provided, used what was calculated.
	if (ifltab[zdssKeys.kopenMode] == 0) {
		ifltab[zdssKeys.kopenMode] = ifltab[zdssKeys.kmultiUserAccess];
	}

	status = zcheckKeys(ifltab);
	if (zisError(status)) {
		free((long long*)ifltab[zdssKeys.kfileHeader]);
		free((long long*)ifltab[zdssKeys.kpathBin]);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zopen_ID);
	}
	return status;
}

