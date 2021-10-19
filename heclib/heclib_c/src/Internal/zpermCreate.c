
#include <stdio.h>
#include <stdlib.h>

#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "heclib7.h"
#include "hecdssInternal.h"

/**
*  Function:	zpermCreate
*
*  Use:			Private (Internal)
*
*  Description:	Creates and writes the permanent section (header) of a new DSS file.
*
*  Declaration: int zpermCreate (long long *ifltab, int maxExpectedPathnames, int hashSize, int binSize)
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				int maxExpectedPathnames (Optional)
*					Sets the size of the internal tables to be optimum with then number of pathnames specified
*					The DSS file will work with any number of pathnames, this just optimizes speed and space.
*					To set to the default, set to 0 (zero).
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
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for invalid operations detected by zopen.
*					error number returned by system open function (e.g., _sopen_s for Windows)
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zpermCreate (long long *ifltab, int maxExpectedPathnames, int hashSize, int binSize)
{

	int status;
	int size;
	int atEOF;
	char messageString[80];
	long long *fileHeader;
	int zero[2];

	if (zmessageLevel(ifltab, MESS_METHOD_PERM_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zpermCreate_ID, "Enter", "");
	}


	zero[0] = 0;
	zero[1] = 0;
	status = zcheckKeys(ifltab);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermCreate_ID);
	}

	//  Allocate memory for the file header first
	ifltab[zdssKeys.kfiHeadSize] = zdssFileKeys.kendFileHeader + 1;
	if (ifltab[zdssKeys.kfiHeadSize] < DSS_MIN_FILE_HEADER_SIZE) {
		ifltab[zdssKeys.kfiHeadSize] = DSS_MIN_FILE_HEADER_SIZE;
	}
	status = zmemoryGet(ifltab, zdssKeys.kfileHeader, (int)ifltab[zdssKeys.kfiHeadSize], "DSS file header");
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermCreate_ID);
	}
	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	fileHeader[zdssFileKeys.kfileSize] = ifltab[zdssKeys.kfiHeadSize];
	fileHeader[zdssFileKeys.kfileHeaderSize] = ifltab[zdssKeys.kfiHeadSize];

	fileHeader[zdssFileKeys.kmaxExpectedPathnames] = maxExpectedPathnames;
	if ((maxExpectedPathnames == 0) && (zdssVals.maxExpectedPathnames > 0)) {
		maxExpectedPathnames = zdssVals.maxExpectedPathnames;
	}
	zdssVals.maxExpectedPathnames = 0;
	znewFileSize(ifltab, maxExpectedPathnames, hashSize, binSize);

	if (zmessageLevel(ifltab, MESS_METHOD_PERM_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		 _snprintf_s(messageString, sizeof(messageString), _TRUNCATE, 	"%d;     Bin Size: %d",
			 (int)fileHeader[zdssFileKeys.kmaxHash], (int)fileHeader[zdssFileKeys.kbinSize]);
		 zmessageDebug(ifltab, DSS_FUNCTION_zpermCreate_ID, "Hash Size: ", messageString);
	 }

	//  Allocate memory for the pathname bin, etc.
	//  Pathname Bin
	status = zmemoryGet(ifltab, zdssKeys.kpathBin, (int)fileHeader[zdssFileKeys.kbinSize], "Pathname bin");
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermCreate_ID);
	}

	//  Record header (info area)
	status = zmemoryGet(ifltab, zdssKeys.kinfo, zdssVals.maxInfoSize, "Record header (info area)");
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermCreate_ID);
	}

	//  KDSS points to the identifier 'ZDSS', indicating that this is a DSS file
	//  "ZDSS" is always the very start of the DSS file
	charInt((void *)zdssVals.czdss, (void *)&fileHeader[zdssFileKeys.kdss], 5, 8, 1, 1, 0);


/*     KVERS is the DSS software version for this file
 *     KVERS must always remain in the same location in the file
 *     so past and future versions can recognize the file as DSS
 *     For version 7, skip an additional word to keep compatible
 */
	charInt((void *)zdssVals.czVersion, (void *)&fileHeader[zdssFileKeys.kversion], 5, 8, 1, 1, 0);


/*  these point to the number of records in the file */
    fileHeader[zdssFileKeys.knumberRecords] = 0;
	fileHeader[zdssFileKeys.knumberExpansions] = 0;
	fileHeader[zdssFileKeys.knumberRenames] = 0;
	fileHeader[zdssFileKeys.knumberDeletes] = 0;
	fileHeader[zdssFileKeys.knumberAliases] = 0;

/*     kdead is the dead space pointer */
    fileHeader[zdssFileKeys.kdead] = 0;

/*     kcreateDate pointes to the date the file was created */
    fileHeader[zdssFileKeys.kcreateDate] = getCurrentTimeMillis();

/*     klastWriteTime points to the date/time the file was last written to */
    fileHeader[zdssFileKeys.klastWriteTime] = getCurrentTimeMillis();
	ifltab[zdssKeys.kmyLastWriteTime] = fileHeader[zdssFileKeys.klastWriteTime];

/*     kaddHashTableStart points to the address of the hash table */
    fileHeader[zdssFileKeys.kaddHashTableStart] = 0;

/*     kbinsRemainInBlock points to the number of bins remaining in the current block */
     fileHeader[zdssFileKeys.kbinsRemainInBlock] = 0;

/*     kaddFirstBin points to the location of the first bin */
    fileHeader[zdssFileKeys.kaddFirstBin] = 0;

/*     kaddNextEmptyBin points to the location of the next empty bin */
    fileHeader[zdssFileKeys.kaddNextEmptyBin] = 0;


/*     File efficiency variables */

/*     ktotalBins indicates the number of pathname bins used in the file */
    fileHeader[zdssFileKeys.ktotalBins] = 0;

/*     khashsUsed are the number of hash codes used.  This will always be */
/*     less than or equal to kmaxHash */
    fileHeader[zdssFileKeys.khashsUsed] = 0;

/*     kbinsOverflow is the number of overflow bins - previous bins filled */
    fileHeader[zdssFileKeys.kbinsOverflow] = 0;

/*     kmaxPathsOneHash is the maximum number of pathnames for any one bin */
    fileHeader[zdssFileKeys.kmaxPathsOneHash] = 0;

/*     kmaxPathsHashCode is the hash code for this */
    fileHeader[zdssFileKeys.kmaxPathsHashCode] = 0;

/*     kfilePassword points to the file password (encoded) */
    fileHeader[zdssFileKeys.kfilePassword] = 0;

	//  kreclaimSize is the amount of space set aside on disk to hold
	//  reclaimed space info.  It is 2 X the possible number, with the
	//  first element being the size and the second being the file address.
	fileHeader[zdssFileKeys.kreclaimSize] = 1002;
	fileHeader[zdssFileKeys.kreclaimNumber] = (fileHeader[zdssFileKeys.kreclaimSize] - 2) / 2;   //  Last one points to address of next array
	fileHeader[zdssFileKeys.kreclaimMin] = 100;
	fileHeader[zdssFileKeys.kreclaimSegNumber] = 0;
	fileHeader[zdssFileKeys.kreclaimMaxSegment] = 20;  //  20 * 500 = 10,000 locations available
	fileHeader[zdssFileKeys.kreclaimSegmentsUsed] = 0;

/*   maximum lengths of pathname parts */
    fileHeader[zdssFileKeys.kmaxPath] = 0;
	fileHeader[zdssFileKeys.kmaxA] = 0;
	fileHeader[zdssFileKeys.kmaxB] = 0;
	fileHeader[zdssFileKeys.kmaxC] = 0;
	fileHeader[zdssFileKeys.kmaxD]= 0;
	fileHeader[zdssFileKeys.kmaxE] = 0;
	fileHeader[zdssFileKeys.kmaxF] = 0;

	/*  Maximum record part lengths  */
	fileHeader[zdssFileKeys.kmaxInternalHeader] = 0;
	fileHeader[zdssFileKeys.kmaxHeader2] = 0;
	fileHeader[zdssFileKeys.kmaxUserHeader] = 0;
	fileHeader[zdssFileKeys.kmaxValues1Size] = 0;
	fileHeader[zdssFileKeys.kmaxValues2Size] = 0;
	fileHeader[zdssFileKeys.kmaxValues3Size] = 0;
	fileHeader[zdssFileKeys.kmaxRecordSize] = 0;

	fileHeader[zdssFileKeys.kendFileHeader] = DSS_END_HEADER_FLAG;

	//  Indicate that the file is locked, even though it is not
	//  We always need a lock when we write, except when creating
	//  a new file.  This will bypass check in zput
	ifltab[zdssKeys.klocked] = 1;

	//  Write these initial values to the permanent area (file header)
	status = zpermWrite(ifltab);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermCreate_ID);
	}


	/*
	Now set aside space for the lock arrays, which immediately follow
		the permanent section.
	1.  The first word (101) is the primary "lock word".  Whoever has
		this locked has the entire file "write locked"  (others still may
		look at it, but no one else can write, and it is not guaranteed
		to be current.
	2.  The second word (102) is the "Exclusive lock" word.  If a process
		has this word locked, then they have exclusive access (via lock)
		to the file.  This usually happens during a squeeze or file copy.
		Typically, the open is exclusive, not the lock.
	3.  Following this is the "lock array", currently set to 100 words.
		When a process wants to first write to a file, the first non-locked
		word from the lock array is locked (then the lock or request takes
		place).  This lets other process know that this process is writing
		to the file, and they must go directly into multi-user access mode.
	4.  Following the lock array is the "read lock" array, also 100 words.
		When a process access a file, it is to lock the first non-locked
		word from the read lock array.  This tells other process that this
		process is looking at the file, and they cannot have exclusive access
		and should update (flush) the file more often.  A process that will
		write to the file must ALSO do a read lock.
	5.  Following the read lock array is the write process ID array, also 100 words.
		When a process first writes to the file, it is to record its process ID
		and access mode in the first empty word of the process ID array.
		This array is for informational purposes only; someone can see
		how many processes are accessing the file and their access modes.
		When a processes access mode changes, their PID mode is to be
		updated too.  The actual word is
			PID word = (ifltab[kmultiUser] * 10,000,000) + PID
		If a process opens the file for writing, and there are no other
		users, then the process ID array is cleared, as it is anticipated there
		will be stale PIDs from programs that do not close the file, where
		the PID word is removed.

	*/
	//  Make space for the primary lock words
	//  Note, we have one extra unused spot (for compatibility)
	fileHeader[zdssFileKeys.klockAddressWord] = zgetFileSpace(ifltab, 3, 0, &atEOF);
	status = zput(ifltab, fileHeader[zdssFileKeys.klockAddressWord], zero, -3, 2);
	ifltab[zdssKeys.klockExclusiveWord] = fileHeader[zdssFileKeys.klockAddressWord] + 1;

	//  Space for the write lock area
	fileHeader[zdssFileKeys.klockArraySizes] = DSS_LOCK_ARRAY_SIZE;
	fileHeader[zdssFileKeys.klockWriteArrayAddress] = zgetFileSpace(ifltab, (int)fileHeader[zdssFileKeys.klockArraySizes], 0, &atEOF);
	status = zput(ifltab, fileHeader[zdssFileKeys.klockWriteArrayAddress] , zero, -(int)fileHeader[zdssFileKeys.klockArraySizes], 2);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermCreate_ID);
	}

	//  Space for the read lock area
	fileHeader[zdssFileKeys.klockReadArrayAddress] = zgetFileSpace(ifltab, (int)fileHeader[zdssFileKeys.klockArraySizes], 0, &atEOF);
	status = zput(ifltab, fileHeader[zdssFileKeys.klockReadArrayAddress] , zero, -(int)fileHeader[zdssFileKeys.klockArraySizes], 2);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermCreate_ID);
	}

	//  Space for process ids
	fileHeader[zdssFileKeys.kpidArrayAddress] = zgetFileSpace(ifltab, (int)fileHeader[zdssFileKeys.klockArraySizes], 0, &atEOF);
	status = zput(ifltab, fileHeader[zdssFileKeys.kpidArrayAddress] , zero, -(int)fileHeader[zdssFileKeys.klockArraySizes], 2);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermCreate_ID);
	}


	//  Now, write additional pieces, such as the hash table and first pathname bin

	//  Write the empty hash table
	if (zmessageLevel(ifltab, MESS_METHOD_PERM_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		 zmessageDebug(ifltab, DSS_FUNCTION_zpermCreate_ID, "Creating primary hash table", "");
	}
	size = (int)fileHeader[zdssFileKeys.kmaxHash];
	fileHeader[zdssFileKeys.kaddHashTableStart] = zgetFileSpace(ifltab, size, 0, &atEOF);
	status = zput(ifltab, fileHeader[zdssFileKeys.kaddHashTableStart], zero, -size, 2);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermCreate_ID);
	}

	//  Write the empty reclaimed space table
	if (zmessageLevel(ifltab, MESS_METHOD_PERM_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zpermCreate_ID, "Creating first reclaimed space table", "");
	}
	size = (int)fileHeader[zdssFileKeys.kreclaimSize];
	fileHeader[zdssFileKeys.kreclaimTableAddress] = zgetFileSpace(ifltab, size, 0, &atEOF);
	fileHeader[zdssFileKeys.kreclaimSegAvailableAdd] = fileHeader[zdssFileKeys.kreclaimTableAddress];
	status = zput(ifltab, fileHeader[zdssFileKeys.kreclaimTableAddress], zero, -size, 2);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermCreate_ID);
	}
	fileHeader[zdssFileKeys.kreclaimTotal] = 0;

	//  Write the first pathname hash bin set
	if (zmessageLevel(ifltab, MESS_METHOD_PERM_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		 zmessageDebug(ifltab, DSS_FUNCTION_zpermCreate_ID, "Creating first bin set", "");
	}
	//  One extra space at the end of the bin blocks to point to the next blocks
	size = (int)(fileHeader[zdssFileKeys.kbinSize] * fileHeader[zdssFileKeys.kbinsPerBlock]) + 1;
	fileHeader[zdssFileKeys.kaddFirstBin] = zgetFileSpace(ifltab, size, 0, &atEOF);
	fileHeader[zdssFileKeys.kaddNextEmptyBin] = fileHeader[zdssFileKeys.kaddFirstBin];
	status = zput(ifltab, fileHeader[zdssFileKeys.kaddFirstBin], zero, -size, 2);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermCreate_ID);
	}
	fileHeader[zdssFileKeys.kbinsRemainInBlock] = fileHeader[zdssFileKeys.kbinsPerBlock];

	//  Write the EOF flag
	status = zwriteEOF(ifltab);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermCreate_ID);
	}

	//  Write updated header to disk (includes hash table, path bin)
	status = zpermWrite(ifltab);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermCreate_ID);
	}

	//  Force flush to disk
	status = zflushToDisk(ifltab, 1);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermCreate_ID);
	}

	//  reset lock flag
	ifltab[zdssKeys.klocked] = 0;
	ifltab[zdssKeys.kwritingNow] = 0;

	status = zcheckKeys(ifltab);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zpermCreate_ID);
	}

	if (zmessageLevel(ifltab, MESS_METHOD_PERM_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zpermCreate_ID, "Exit, status: ", status);
	}

	return status;
}

