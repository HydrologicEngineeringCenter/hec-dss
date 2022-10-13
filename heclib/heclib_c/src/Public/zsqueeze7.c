#ifdef _MSC_VER
#include <io.h>
#include <errno.h>
#include <windows.h>
#else
#include <unistd.h>
#include <stdint.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "heclib.h"
#include "zdssKeys.h"
#include "zprogress.h"
#include "hecdssInternal.h"
#include "zdssLocking.h"
#include "zdssMessages.h"

#ifndef _MSC_VER
#ifndef MAX_PATH
#define MAX_PATH 300
#endif

#ifndef TCHAR
#define TCHAR char*
#endif
#endif

/**
*  Function:	zsqueeze7
*
*  Use:			Public zsqueeze7
*
*  Description:	Squeezes a DSS version 7 file.  A squeeze rebuilds a file, which removes inactive space,
*					rebuilds internal tables and adjust table sizes to optimize data access.
*					Squeezing uses a brute force approach, which will recover any data sets that may
*					have broken links (rare), usually from a crash or disk damage.
*					This is similar to de-fragmenting your file system.��
*					Once a squeeze has been accomplished, deleted data cannot be recovered.
*
*  Declaration: int zsqueeze7(long long *ifltab, int boolOnlyIfNeeded, int boolInPlace);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				int boolOnlyIfNeeded
*					Set to 1 to squeeze only if the file needs to be squeezed
*					Set to 0 to force a squeeze.
*
*				int boolInPlace
*					Set to 1 to force an in-place squeeze, retaining ownership, etc.
*					Copies back so the same file is used.  This takes twice as long as a normal squeeze.
*					Set to 0 for a normal squeeze, where a file rename takes place (preferred)
*
*
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode (< 0) should an error occur.
*
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zsqueeze7(long long *ifltab, int boolOnlyIfNeeded, int boolInPlace)
{

	long long ifltabTemp[400] = {0};

	char fullFilename[MAX_PATH] = {0};
	char filename[MAX_PATH] = {0};
	TCHAR tempDirW[MAX_PATH] = {0};
	char tempName[MAX_PATH] = {0};
	size_t len = 0;
	int ihandle = 0;
	int access = 0;
	int status = 0;
	int boolMessageLevelSet = 0;
	int numberPathnames = 0;
	int boolNeedSqueeze = 0;
	int version = 0;
	int i = 0;
	long long *fileHeader = NULL;



	//  Check for correct DSS Version
	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
								zgetVersion((void *)ifltab), 0, zdssErrorSeverity.WARNING, "", "");
	}


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	status = zcheckKeys(ifltab);
	if (zisError(status)) {
		status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zsqueeze_ID);
		return status;
	}

	charLong((void *)ifltab[zdssKeys.kfullFilename], fullFilename, 0, sizeof(fullFilename), 0, 1);
	charLong((void *)ifltab[zdssKeys.kfilename], filename, 0, sizeof(filename), 0, 1);
	ihandle = (int)ifltab[zdssKeys.khandle];
	access = (int)ifltab[zdssKeys.kmultiUserAccess];
	numberPathnames = (int)fileHeader[zdssFileKeys.kmaxExpectedPathnames];
	if (numberPathnames <= 0) numberPathnames = (int)fileHeader[zdssFileKeys.knumberRecords];

	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zsqueeze_ID, "Enter, file name: ", fullFilename);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zsqueeze_ID, "Handle: ", zhandle(ifltab));
		zmessageDebugInt(ifltab, DSS_FUNCTION_zsqueeze_ID, "boolOnlyIfNeeded: ", boolOnlyIfNeeded);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zsqueeze_ID, "boolInPlace: ", boolInPlace);
	}

	//  First, let's determine if this file can be squeezed?
	//  Is someone else accessing the file?
	//  The following takes care of a lock structure change in test versions.
	version = zgetFullVersion(ifltab);
	if (version > 70300) {
		status = zlockPassive(ifltab, LOCKING_LOCK_TEST, LOCKING_ACCESS_READ);
		//  If status > 0, another process is accessing file and we cannot squeeze it
		if (zisError(status)) {
			//  Cannot get exclusive access
			return zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID, zdssErrorCodes.CANNOT_SQUEEZE,
				status, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "",
				"File in use by another process");
		}
	}

	 //  Okay, we can squeeze.
	 //  First, let's see if we should
	 if (boolOnlyIfNeeded) {
		  boolNeedSqueeze = zsqueezeNeeded(ifltab);
		 if (boolNeedSqueeze == 0) {
			 //  Don't need
			 if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_USER_DIAG)) {
				zmessageDebug(ifltab, DSS_FUNCTION_zsqueeze_ID, "Squeeze is not needed for file: ", fullFilename);
			 }
			 return STATUS_OKAY;
		 }
	 }

	 //  We will squeeze

	//  Put the file in exclusive access
	ifltab[zdssKeys.kmultiUserAccess] = EXCLUSIVE_ACCESS;
	zpidUpdate(ifltab);
	status = zlockActive(ifltab, LOCKING_LEVEL_SUPER, LOCKING_LOCK_ON, LOCKING_FLUSH_OFF);
	if (zisError(status)) {
		//  Cannot get exclusive access
		return zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID, zdssErrorCodes.CANNOT_SQUEEZE,
								status, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "",
								"File in use by another process");
	}

	//  Unless we are in debug mode, turn down trace to avoid large, unnecessary output
	boolMessageLevelSet = 1;
	for (i=1; i<NUMBER_METHOD_NAMES; i++) {
		if (zmessaging.methodLevel[i] != MESS_LEVEL_GENERAL) {
			boolMessageLevelSet = 0;
			break;
		}
	}
	if (boolMessageLevelSet) {
		zsetMessageLevel(MESS_METHOD_GENERAL_ID, MESS_LEVEL_CRITICAL);
	}


	//  If we are going to squeeze in place, then use a temporary directory,
	//  otherwise use the same directory

	if (boolInPlace) {
		//  We need a temporary location to create the new (squeezed) file.
		tempDirW[0] = 0;
#ifdef _MSC_VER
		status = GetTempPath(MAX_PATH, tempDirW);  // includes backslash.
		if ((status <= 0) || (tempDirW[0] == 0)) {
			if (boolMessageLevelSet) zresetMessageLevel();
			return zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID, zdssErrorCodes.CANNOT_SQUEEZE,
				status, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "",
				"Cannot access temporary directory.");
		}
		wcstombs_s(&len, tempName, MAX_PATH, tempDirW, _TRUNCATE);
		//printf("-->%s<--\n", tempName);
#else
		stringCopy(tempName, sizeof(tempName), "/tmp/", 5);
#endif
		stringCat(tempName, sizeof(tempName), filename, strlen(filename));
		len = (int)strlen(tempName);
	    if (len < 5) {
			if (boolMessageLevelSet) zresetMessageLevel();
		   printf("File does not exsit or cannot access. name: %s\n", fullFilename);
			return zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID,
				zdssErrorCodes.UNABLE_TO_CREATE_FILE, 0, 0,
				zdssErrorSeverity.WARNING_NO_WRITE_ACCESS, "", tempName);
		}
		tempName[len-4] = '\0';
		stringCat(tempName, sizeof(tempName), "_squ.dss", 8);
		//printf("-->%s<--\n", tempName);

		//  Just in case the temporary file already exists...
#ifdef _MSC_VER
		_unlink(tempName);
		_set_errno(0);
#else
		unlink(tempName);
#endif
	}
	else {
		stringCopy(tempName, sizeof(tempName), fullFilename, strlen(fullFilename));
	    len = (int)strlen(tempName);
	    if (len < 5) {
			if (boolMessageLevelSet) zresetMessageLevel();
		   printf("File does not exsit or cannot access. name: %s\n", fullFilename);
			return zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID,
				zdssErrorCodes.UNABLE_TO_CREATE_FILE, 0, 0,
				zdssErrorSeverity.WARNING_NO_WRITE_ACCESS, "", tempName);
		}
		tempName[len-4] = '\0';
		stringCat(tempName, sizeof(tempName), "_squ.dss", 8);

		//  Be sure the file does not exist, just in case one is left over from a prior squeeze
#ifdef _MSC_VER
		_unlink(tempName);
		_set_errno(0);
#else
		unlink(tempName);
#endif

		//  Close the DSS file so we can try a rename
		status = zclose(ifltab);
		if (zisError(status)) {
			status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zsqueeze_ID);
			if (boolMessageLevelSet) zresetMessageLevel();
			return status;
		}

		//  Rename the current file to the temp name, just to be sure we can do this step
		//  (This is a common failure point, mainly due to permission or similar)
		status = rename(fullFilename, tempName);
		if (zisError(status)) {
			if (boolMessageLevelSet) zresetMessageLevel();
			//  Print a nice message, as this may happen often
			printf("Unable to rename file for squeezing, file: %s\n", fullFilename);
		    return zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID,
				zdssErrorCodes.UNABLE_TO_CREATE_FILE, 0, 0,
				zdssErrorSeverity.WARNING_NO_WRITE_ACCESS, "", tempName);
	   }

	   //  Almost always able to rename back
	   status = rename(tempName, fullFilename);
	   if (zisError(status)) {
		   if (boolMessageLevelSet) zresetMessageLevel();
		   //  Print a nice message, as this may happen often
		   printf("Unable to rename back file for squeezing, file: %s\n", tempName);
		   return zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID,
				zdssErrorCodes.UNABLE_TO_CREATE_FILE, 0, 0,
				zdssErrorSeverity.WARNING_NO_WRITE_ACCESS, "", tempName);
	   }

	   //  Reopen the DSS file
	   status = zopenInternal(ifltab, fullFilename, EXCLUSIVE_ACCESS, 0, 0, 0, 0);
	   if (zisError(status)) {
		   if (boolMessageLevelSet) zresetMessageLevel();
			status = zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID, zdssErrorCodes.CANNOT_SQUEEZE,
								status, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "", fullFilename);
			return status;
		}

	}


	//  Now open the temporary DSS file
	status = zopenInternal(ifltabTemp, tempName, EXCLUSIVE_ACCESS, numberPathnames, 0, 0, 0);
	if (zisError(status)) {
		if (boolMessageLevelSet) zresetMessageLevel();
		status = zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID, zdssErrorCodes.CANNOT_SQUEEZE,
							status, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "",
							"Cannot create temporary file");
		zmessageInterface(ifltab, tempName, 0);
		return status;
	}

	//  Now copy the current file to the temp file, rebuilding
	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zsqueeze_ID, "Copying records to temp file ", tempName);
	}
	//////////////    Main Copy   //////////////

	///////////////////////////////////////////////
	//////////////////////////////////////////////
	////  fix me here.  If invalid open, then copy as damage file and pass in ifltab.
	status = zcopyFile(ifltab, ifltabTemp, 0);
	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zsqueeze_ID, "Afer copying to temp file, status: ", status);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zsqueeze_ID, "Interrupt: ", zprogress.interrupt);
	}
	if (zprogress.interrupt != 0) {
		zclose(ifltabTemp);
#ifdef _MSC_VER
		_unlink(tempName);
		_set_errno(0);
#else
		unlink(tempName);
#endif
		return 0;
	}
	if (zisError(status)) {
		if (boolMessageLevelSet) zresetMessageLevel();
		printf("Error during squeeze. name: %s\n", fullFilename);
		zclose(ifltabTemp);
#ifdef _MSC_VER
		_unlink(tempName);
		_set_errno(0);
#else
		unlink(tempName);
#endif
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zsqueeze_ID);
	}
	zflushToDisk (ifltabTemp, 1);

	//  Validate that the new file is complete and correct
	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zsqueeze_ID, "Vaildating file ", tempName);
	}
	status = zpermRead(ifltabTemp);
	if (zisError(status)) {
		if (boolMessageLevelSet) zresetMessageLevel();
		status = zerrorUpdate(ifltabTemp, status, DSS_FUNCTION_zsqueeze_ID);
		zclose(ifltabTemp);
#ifdef _MSC_VER
		_unlink(tempName);
		_set_errno(0);
#else
		unlink(tempName);
#endif
		return status;
	}

	//  Make sure the squeezed file is good
	status = zcheckPathnameBins(ifltabTemp);
	if (zisError(status)) {
		if (boolMessageLevelSet) zresetMessageLevel();
		status = zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID, zdssErrorCodes.CANNOT_SQUEEZE,
							status, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "",
							"Incomplete update, file not squeezed.");
		zclose(ifltabTemp);
#ifdef _MSC_VER
		_unlink(tempName);
		_set_errno(0);
#else
		unlink(tempName);
#endif
		return status;
	}


	//  At this point we have the squeezed file in a temporary location
	//  We can either delete the old one and then rename the temporary to
	//  the old one name,
	//  or we can do a wholesale copy of the squeezed file into the old file

	if (boolInPlace) {

		//  Release the lock so we can replace file contents
		zlockPassive(ifltab, LOCKING_LOCK_OFF, LOCKING_ACCESS_READ);
		ifltab[zdssKeys.kmultiUserAccess] = RELEASE_ACCESS;
		zpidUpdate(ifltab);
		status =  zlockActive(ifltab, LOCKING_LEVEL_SUPER, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
		if (status != 0) {
			if (boolMessageLevelSet) zresetMessageLevel();
			status = zerrorUpdate(ifltabTemp, status, DSS_FUNCTION_zsqueeze_ID);
			zclose(ifltabTemp);
#ifdef _MSC_VER
			_unlink(tempName);
			_set_errno(0);
#else
			unlink(tempName);
#endif
			return status;
		}

	//  Now remove the contents of the original file
#ifdef _MSC_VER
		status = (int)_lseeki64(ihandle, (long long)0, (long long)0);
		status = _chsize(ihandle, (long long)0);
#else
		status = ftruncate(ihandle, (long long)0);
#endif
		if (status != 0) {
			zclose(ifltabTemp);
#ifdef _MSC_VER
			_unlink(tempName);
			_set_errno(0);
#else
			unlink(tempName);
#endif
			if (boolMessageLevelSet) zresetMessageLevel();
			return zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID, zdssErrorCodes.CANNOT_SQUEEZE,
								status, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "",
								"Incomplete update, file not squeezed.");
		}

		//  Copy the new file directly to the original file
		if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zsqueeze_ID, "Moving squeezed file back to origial file ", fullFilename);
		}
		status = copyFile(ifltabTemp, ihandle);
		if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zsqueeze_ID, "Move complete, status: ", status);
		}
		if (status != 0) {
			zclose(ifltabTemp);
			if (boolMessageLevelSet) zresetMessageLevel();
			return zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID, zdssErrorCodes.CANNOT_SQUEEZE,
								status, 0, zdssErrorSeverity.WRITE_ERROR, "",
								"File copied failed, file is in temp directory.");
		}

		//  Flush the (new) original file
		status = flushFile(ihandle);

		//  Restore access mode
		ifltab[zdssKeys.kmultiUserAccess] = access;
		zlockPassive(ifltab, LOCKING_LOCK_ON, LOCKING_ACCESS_READ);
		zlockActive(ifltab, LOCKING_LEVEL_SUPER, LOCKING_LOCK_ON, LOCKING_FLUSH_OFF);
		zpidUpdate(ifltab);
		zflushToDisk (ifltab, 0) ;

		//  Check that the copied file is good
		status = zpermRead(ifltab);
		if (status != 0) {
			if (boolMessageLevelSet) zresetMessageLevel();
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zsqueeze_ID);
		}

		//  After all that we have done, an error beyond this point is highly unlikely
		status = zcheckPathnameBins(ifltab);
		if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_USER_DIAG)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zsqueeze_ID, "Final validation complete, status ", status);
		}
		if (status != 0) {
			if (boolMessageLevelSet) zresetMessageLevel();
			return zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID, zdssErrorCodes.CANNOT_SQUEEZE,
									status, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "",
									"Incomplete update, file failed check.");
		}

		zlockActive(ifltab, LOCKING_LEVEL_SUPER, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);

		//  Close and delete the temp file (not a biggie if these fail.)
		zclose(ifltabTemp);
#ifdef _MSC_VER
		_unlink(tempName);
		_set_errno(0);
#else
		unlink(tempName);
#endif

	}
	else {

		//  Don't do in place - delete the old file
		//  and rename the temp file to the old file.
		//  close both files
		status = zclose(ifltab);
		if (zisError(status)) {
			if (boolMessageLevelSet) zresetMessageLevel();
			status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zsqueeze_ID);
			return status;
		}
		status = zclose(ifltabTemp);
		if (zisError(status)) {
			if (boolMessageLevelSet) zresetMessageLevel();
			status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zsqueeze_ID);
			return status;
		}

		//  Delete the old file
#ifdef _MSC_VER
		status = _unlink(fullFilename);
#else
		status = unlink(fullFilename);
#endif
	    if (zisError(status)) {
			if (boolMessageLevelSet) zresetMessageLevel();
		   printf("Error during squeeze. Unable to delete file.  name: %s\n", fullFilename);
		   return zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID,
				zdssErrorCodes.UNABLE_TO_CREATE_FILE, 0, 0,
				zdssErrorSeverity.WARNING_NO_WRITE_ACCESS, "", fullFilename);
	   }

	   //  Rename temp to old file
	   status = rename(tempName, fullFilename);
	   if (zisError(status)) {
		   if (boolMessageLevelSet) zresetMessageLevel();
		   //  Print a nice message, as this may happen often
		   printf("Unable to rename back file after squeezing, file: %s\n", tempName);
		   return zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID,
				zdssErrorCodes.UNABLE_TO_CREATE_FILE, 0, 0,
				zdssErrorSeverity.WARNING_NO_WRITE_ACCESS, "", tempName);
	   }

	   //  Reopen file
		status = zopenInternal(ifltab, fullFilename, access, 0, 0, 0, 0);
		if (zisError(status)) {
			if (boolMessageLevelSet) zresetMessageLevel();
			status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zsqueeze_ID);
			return status;
		}
	}

	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zsqueeze_ID, "Squeeze complete, status ", status);
	}

	//  Reset the message level
	if (boolMessageLevelSet) zresetMessageLevel();

	return status;
}

