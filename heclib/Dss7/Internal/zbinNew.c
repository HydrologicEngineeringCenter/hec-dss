#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"


/**
*  Function:	zbinNew
*
*  Use:			Private (internal)
*
*  Description:	Creates a new "pathname bin" in a "bin block".  If the bin block is full,
*					this creates a new block at the end of the file
*
*  Declaration: zbinNew(long long *ifltab)
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for error
*
*
*	Called by:	zwriteNew and zbinUpdate
*
*	See Also:	zbinUpdate
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zbinNew(long long *ifltab)
{
	int i;
	int status;
	int isize;
	int blockSize;
	int atEOF;

	long long *pathnameBin;
	long long izero = 0;
	long long address;
	long long newBinAddress;
	long long *fileHeader;


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zbinNew_ID, "Enter", "");
	}

	status = STATUS_OKAY;

	//  Get the memory location of the pathname bin for this file
	pathnameBin = (long long *)ifltab[zdssKeys.kpathBin];
	if (pathnameBin == 0) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_zbinNew_ID);
	}
	// Record that we have created a new bin
	fileHeader[zdssFileKeys.ktotalBins]++;

	newBinAddress = 0;
	isize = (int)fileHeader[zdssFileKeys.kbinSize];
	//  Is there a pathname bin for this table hash?
	//  (The table hash will have already been loaded by zcheck)
	if (ifltab[zdssKeys.khashTableBinAdd] != 0) {
		//  Yes, read it.
		status = zget(ifltab, ifltab[zdssKeys.kbinAddCurrent], (int *)pathnameBin, isize, 2);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zbinNew_ID);
		}
		//  Are there any more unused (empty) pathname bins in this block?
		//  (A "block" will contain many (e.g., 32) pathname bins together)
		if (fileHeader[zdssFileKeys.kbinsRemainInBlock] <= 0) {
			//  No more space in this block, create a new block at the end of the file
			//  Compute space needed -
			//  One extra word at the end of the bin block to point to the next block
			blockSize = (int)(fileHeader[zdssFileKeys.kbinSize] * fileHeader[zdssFileKeys.kbinsPerBlock]) + 1;
			newBinAddress = zgetFileSpace(ifltab, blockSize, 0, &atEOF);
			pathnameBin[isize-1] = newBinAddress;
		}
		else {
			//  Yes, there is a unused bin in this block
			pathnameBin[isize-1] = fileHeader[zdssFileKeys.kaddNextEmptyBin];
		}
		//  Save the address of this new bin in the current (full) bin (for this hash)
		address = ifltab[zdssKeys.kbinAddCurrent] + isize - 1;
		status = zput(ifltab, address, (int *)&pathnameBin[isize-1], 1, 2);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zbinNew_ID);
		}
	}
	//  No empty bins remain in this block; create new block at EOF
	if (fileHeader[zdssFileKeys.kbinsRemainInBlock] <= 0) {
		//  Write a new pathname block at the end of the file
		//  First, set the address of this in the last block
		if (newBinAddress == 0) {
			blockSize = (int)(fileHeader[zdssFileKeys.kbinSize] * fileHeader[zdssFileKeys.kbinsPerBlock]) + 1;
			newBinAddress = zgetFileSpace(ifltab, blockSize, 0, &atEOF);
		}
		status = zput(ifltab, fileHeader[zdssFileKeys.kaddNextEmptyBin], (int *)&newBinAddress, 1, 2);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zbinNew_ID);
		}
		//  Set location of current block at end of file
		fileHeader[zdssFileKeys.kbinsRemainInBlock] = fileHeader[zdssFileKeys.kbinsPerBlock] - 1;
		ifltab[zdssKeys.kbinAddCurrent] = newBinAddress;
		if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
			zmessageDebug(ifltab, DSS_FUNCTION_zbinNew_ID, "Writing new block of pathname bins at end of file", "");
		}
		status = zput(ifltab, ifltab[zdssKeys.kbinAddCurrent], (int *)&izero, -blockSize, 2);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_zbinNew_ID);
		}
		fileHeader[zdssFileKeys.kaddNextEmptyBin] = ifltab[zdssKeys.kbinAddCurrent] + fileHeader[zdssFileKeys.kbinSize];
		//  Write the EOF flag
		if (atEOF) {
			status = zwriteEOF(ifltab);;
		}
	}
	else {
		//  There are still some empty bins in this block; use one
		fileHeader[zdssFileKeys.kbinsRemainInBlock]--;
		ifltab[zdssKeys.kbinAddCurrent] = fileHeader[zdssFileKeys.kaddNextEmptyBin];
		//  When we've reached the end of the bin block, this will be used to point to next bin block
		fileHeader[zdssFileKeys.kaddNextEmptyBin] += fileHeader[zdssFileKeys.kbinSize];
	}
	//  Be sure the new bin is clear
	for (i=0; i<(int)fileHeader[zdssFileKeys.kbinSize]; i++) {
		pathnameBin[i] = 0;
	}
	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zbinNew_ID, "Exit", "");
	}

	return status;
}

