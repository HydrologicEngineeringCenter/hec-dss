#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdlib.h>

#include "heclib7.h"
#include "heclib6.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"



/**
*  Function:	zrename
*
*  Use:			Public
*
*  Description:	Renames a single DSS record
*
*  Declaration: int zrename(long long *ifltab, const char* oldPathname, const char* newPathname);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				const char *oldPathname
*					The pathname of the record to be renamed.
*
*				const char *newPathname
*					The pathname to rename the record to.
*
*
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					STATUS_RECORD_NOT_FOUND for record not found
*					errorCode for invalid operations detected by zopen.
*					error number returned by system open function (e.g., _sopen_s for Windows)
*
*	Note:		There are several record types that depend on pathname parts that cannot be
*					renamed.  This function does not check those parts and renaming them could
*					make that data inaccessible.  For example, you cannot rename the D or E parts
*					of time-series data.
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


//
int zrename(long long *ifltab, const char* oldPathname, const char* newPathname)
{
	long long *info;
	long long *infoOld;
	long long infoAddress;
	long long address;
	long long binStatus;
	long long eofWord;
	long long icurrentTime;
	int dataType;
	int temp;
	int numberInfoOld;
	int numberInfoNew;
	int wroteAtEOF;
	int numberChars;
	int pathnameSize;
	int istat = 0;
	int status;
	int i;
	int atEOF;
	int len;
	int len1;
	int len2;
	int positions[7];
	int start;
	int end;
	char *newPath;

	long long *fileHeader;


	if (!oldPathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zrename_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "pathnameFrom is null");
	}
	if (!newPathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zrename_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "pathnameTo is null");
	}

	//  Messages and debug
	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zrename_ID, "Enter,  handle: ", zhandle(ifltab));
		zmessageDebug(ifltab, DSS_FUNCTION_zrename_ID, "    Old Pathname: ",oldPathname);
		zmessageDebug(ifltab, DSS_FUNCTION_zrename_ID, "    New Pathname: ",newPathname);
	}

	infoOld = 0;
	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];

	if (ifltab[zdssKeys.kopenStatus] != OPEN_STAT_WRITE) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zrename_ID,
			zdssErrorCodes.WRITE_ON_READ_ONLY, 0, 0,
			zdssErrorSeverity.WARNING_NO_WRITE_ACCESS, oldPathname, newPathname);
	}

	//  Make a copy of the newPathname so that we can change it
	newPath = mallocAndCopyPath(newPathname);
	if (!newPath) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zrename_ID,
			zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
			zdssErrorSeverity.MEMORY_ERROR,
			newPathname, "Allocating space for pathname");
	}

	//  Be sure the new pathname does not already exist
	ifltab[zdssKeys.kaddInfoLastPath] = 0;
	status = zcheckInternal(ifltab, newPathname, 1);
	if (zisError(status)) {
		free(newPath);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zrename_ID);
	}
	if (status == STATUS_RECORD_FOUND) {
		status = zdeleteInternal(ifltab, newPathname, 0);
		if (zisError(status)) {
			free(newPath);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zrename_ID);
		}
		if (status != STATUS_OKAY) {
			if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_GENERAL)) {
				zmessage2(ifltab, ZRENAME_ALREADY_EXIST, newPathname);
				zmessage2(ifltab, ZRENAME_OLD_PATH, oldPathname);
			}
			free(newPath);
			return zerrorProcessing(ifltab, DSS_FUNCTION_zrename_ID,
									zdssErrorCodes.RECORD_ALREADY_EXISTS, 0,
									0, zdssErrorSeverity.WARNING, newPathname, "");
		}
	}

	//  Lock the file if we are in a multi-user access mode
	//  And read the file header
	status =  zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
	if (zisError(status)) {
		free(newPath);
		return zerrorEncodeHigh(status, DSS_FUNCTION_zrename_ID);
	}

	//  Compute the new info block size
	numberInfoNew = zdssVals.infoSize + numberLongsInBytes((int)ifltab[zdssKeys.klenLastPath]);

	//  Force the internals to be loaded during the check
	ifltab[zdssKeys.kaddInfoLastPath] = 0;
	status = zcheckInternal(ifltab, oldPathname, 1);
	if (zisError(status)) {
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
		free(newPath);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zrename_ID);
	}
	if (status != STATUS_RECORD_FOUND) {
		if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_GENERAL)) {
			zmessage2(ifltab, ZRENAME_NOT_EXIST, oldPathname);
		}
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
		free(newPath);
		return STATUS_RECORD_NOT_FOUND;
	}


	//  If the pathname is time series, do not allow the D or E parts to be renamed!
	//  That data has to be transformed, not renamed
	i8toi4(ifltab[zdssKeys.kbinTypeAndCatSort], &dataType, &temp);
	if ((dataType >= DATA_TYPE_RTS) && (dataType < DATA_TYPE_PD)) {
		free(newPath);
		zpathnamePartPositions(newPathname, strlen(newPathname), positions, 7);
		start = positions[5];
		end = positions[6];
		len = (int)strlen(newPathname) + 12;
		newPath = calloc(len, 1);
		stringCopy(newPath, len, newPathname, positions[3]);
		zpathnamePartPositions(oldPathname, strlen(oldPathname), positions, 7);
		stringCat(newPath, len, &oldPathname[positions[3]], (positions[5] - positions[3]));
		stringCat(newPath, len, &newPathname[start], (end - start));
	}


	//  First, mark the pathname bin status as remamed
	address = ifltab[zdssKeys.kpathBinAddress] + zdssBinKeys.kbinStatus;
	binStatus = REC_STATUS_RENAMED;
	status = zput(ifltab, address, (int *)&binStatus, 1, 2);
	if (zisError(status)) {
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
		free(newPath);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zrename_ID);
	}

	//  Read in the old info block
	info = (long long *)ifltab[zdssKeys.kinfo];
	i8toi4(ifltab[zdssKeys.kbinPathLen], &numberChars, &pathnameSize);
	numberInfoOld = zdssVals.infoSize + pathnameSize;
	status = zget(ifltab, ifltab[zdssKeys.kaddInfoLastPath], (int *)info, numberInfoOld, 2);
	if (zisError(status)) {
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
		free(newPath);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zrename_ID);
	}
	//  Save it
	infoAddress = ifltab[zdssKeys.kaddInfoLastPath];
	infoOld = (long long*)calloc((size_t)numberInfoOld, LONG_SIZE);
	if (!infoOld) {
		free(newPath);
		return zerrorProcessing(ifltab, DSS_FUNCTION_zrename_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, numberInfoOld, 0,
								zdssErrorSeverity.MEMORY_ERROR, "", "Allocating infoOld");
	}
	for (i=0; i<numberInfoOld; i++) {
		infoOld[i] = info[i];
	}
	//  Now check to see if the pathname will fit in the old info block.
	if (numberInfoNew > numberInfoOld) {
		//  No - we have to write a new info block at EOF
		//  Mark the old info block as renamed
		info[zdssInfoKeys.kinfoStatus] = REC_STATUS_RENAMED;  //  Mark as renamed
		status = zput(ifltab, ifltab[zdssKeys.kaddInfoLastPath], (int *)info, numberInfoOld, 2);
		if (zisError(status)) {
			if (infoOld) free(infoOld);
			zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
			free(newPath);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zrename_ID);
		}
		fileHeader[zdssFileKeys.kdead] += numberInfoOld;
	}

	//  Save the first and last date from the old info area
	ifltab[zdssKeys.kdataFirstDate] = info[zdssInfoKeys.kinfoFirstDate];
	ifltab[zdssKeys.kdataLastDate] = info[zdssInfoKeys.kinfoLastDate];

	//  Now load the new pathname hash
	status = zcheckInternal(ifltab, newPath, 1);
	if (zisError(status)) {
		if (infoOld) free(infoOld);
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
		free(newPath);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zrename_ID);
	}
	//  If this pathname's hash code was not in the hash table
	//  create a new bin, and save that bin's address in the table
	if (ifltab[zdssKeys.khashTableBinAdd] == 0) {
		//  Write a new pathname bin
		status = zbinNew(ifltab);
		if (zisError(status)) {
			if (infoOld) free(infoOld);
			zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
			free(newPath);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zrename_ID);
		}
		//  Write address of new pathname bin to hash location in table
		ifltab[zdssKeys.khashTableBinAdd] = ifltab[zdssKeys.kbinAddCurrent];
		status = zput(ifltab, ifltab[zdssKeys.kaddTableHash], (int *)&ifltab[zdssKeys.khashTableBinAdd], 1, 2);
		if (zisError(status)) {
			if (infoOld) free(infoOld);
			zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
			free(newPath);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zrename_ID);
		}
		fileHeader[zdssFileKeys.khashsUsed]++;
	}
	else {
		//  Did we stumble on a non-unique pathname hash code?  (rare)
		if (ifltab[zdssKeys.ksameHash]) {
			fileHeader[zdssFileKeys.khashCollisions]++;
			ifltab[zdssKeys.ksameHash] = 0;
		}
	}

	if (numberInfoNew > numberInfoOld) {
		//  The info area for this record will be at EOF
		//  Save the location of info in the pathname bin
		ifltab[zdssKeys.kaddInfoLastPath] = zgetFileSpace(ifltab, numberInfoNew, 0, &atEOF);
		//  Now copy the old info area over new
		for (i=0; i<numberInfoOld; i++) {
			info[i] = infoOld[i];
		}
		wroteAtEOF = 1;
	}
	else {
		//  Info address remains the same, as well as it contents, except path
		ifltab[zdssKeys.kaddInfoLastPath] = infoAddress;
		wroteAtEOF = 0;
	}
	//  Transfer the new pathname and length
	info[zdssInfoKeys.kinfoFlag] = DSS_INFO_FLAG;
	info[zdssInfoKeys.kinfoStatus] = REC_STATUS_PRIMARY;
	info[zdssInfoKeys.kinfoPathnameLength] = ifltab[zdssKeys.klenLastPath];
	info[zdssInfoKeys.kinfoHash] = ifltab[zdssKeys.kpathnameHash];

	//  Store the pathname
	info[numberInfoNew-1] = 0;
	charLong((void *)newPath, (void *)&info[zdssInfoKeys.kinfoPathname], (int)ifltab[zdssKeys.klenLastPath], (int)ifltab[zdssKeys.klenLastPath], 1, 1);

	//  Update the pathname bin area
	i8toi4(info[zdssInfoKeys.kinfoTypeVersion], &dataType, &temp);
	istat = zbinUpdate(ifltab, newPath, ifltab[zdssKeys.kaddInfoLastPath], 1, dataType);

	//  Now write the information area to disk
	ifltab[zdssKeys.kinfoAddress] = ifltab[zdssKeys.kaddInfoLastPath];
	ifltab[zdssKeys.kinfoSize] = numberInfoNew;
	//////////////FIX ME HERE
	status = zput(ifltab, ifltab[zdssKeys.kaddInfoLastPath], (int *)info, numberInfoNew, 2);
	if (zisError(status)) {
		if (infoOld) free(infoOld);
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
		free(newPath);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zrename_ID);
	}
	if (wroteAtEOF == 1) {
		//  Write the EOF flag
		//////////////FIX ME HERE
		eofWord = DSS_END_FILE_FLAG;
		status = zput(ifltab, fileHeader[zdssFileKeys.kfileSize], (int *)&eofWord, 1, 2);
		if (zisError(status)) {
			if (infoOld) free(infoOld);
			zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
			free(newPath);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zrename_ID);
		}
	}

	//  Update file statistics
	zupdatePathStats(ifltab, newPath, (size_t)ifltab[zdssKeys.klenLastPath]);
	fileHeader[zdssFileKeys.knumberRenames]++;
	fileHeader[zdssFileKeys.kcatSortDeletes]++;
	fileHeader[zdssFileKeys.kcatSortNewWrites]++;
	fileHeader[zdssFileKeys.kcatSortStatus] = 3;

	icurrentTime = getCurrentTimeMillis();
	fileHeader[zdssFileKeys.klastWriteTime] = icurrentTime;
	ifltab[zdssKeys.kmyLastWriteTime] = fileHeader[zdssFileKeys.klastWriteTime];

	status = zpermWrite(ifltab);
	if (zisError(status)) {
		if (infoOld) free(infoOld);
		zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_OFF);
		free(newPath);
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zrename_ID);
	}
	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_GENERAL)) {
		zmessage2(ifltab, ZRENAME_OLD_PATH, oldPathname);
		zmessage2(ifltab, ZRENAME_NEW_PATH, newPath);
	}

	if (infoOld) free(infoOld);
	zlockActive(ifltab, LOCKING_LEVEL_MID, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);

	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zrename_ID, "Exit, status: ", status);
	}
	free(newPath);

	return status;
}


//  Fortran compatible interface

void zrenam7_(long long *ifltab, const char* oldPathname, int *nOldPathname,
			  const char* newPathname, int *nNewPathname,
			  int *lfound, size_t lenOldPathname, size_t lenNewPathname)
{
	char *coldPath;
	char *cnewPath;
	int istat;


	coldPath = stringFortToC(oldPathname, lenOldPathname);
	cnewPath = stringFortToC(newPathname, lenNewPathname);

	istat = zrename(ifltab, coldPath, cnewPath);
	if (istat == STATUS_OKAY) {
		*lfound = 1;
	}
	else {
		*lfound = 0;
	}
	free(coldPath);
	free(cnewPath);
}
