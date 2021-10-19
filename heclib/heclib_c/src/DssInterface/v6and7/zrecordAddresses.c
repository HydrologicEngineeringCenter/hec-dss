
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "hecdssInternal.h"

//  DEPRECIATED - USE zgetRecordAddresses INSTEAD
//  This function is here only for version 6 compatibility
//
//  Dimension addresses array to 15
//
int zrecordAddresses(long long *ifltab, const char* pathname, long long *addresses)
{
	long long *info;
	int numberInfo;
	int istat;
	int numberChars;
	 int pathnameSize;
	int status;
	long long *fileHeader;

	/*
	public void setInternals(long internals[])
	{
		tableHash = internals[0];
		hashCode = internals[1];
		hashAddress = internals[2];
		binLength = internals[3];
		binAddress = internals[4];
		infoLength = internals[5];
		infoAddress = internals[6];
		dataLength = internals[7];
		dataAddress = internals[8];
		allocatedSpace = internals[9];
	}
	*/

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	status = zcheck(ifltab, pathname);

	addresses[0] = ifltab[zdssKeys.ktableHash];
	addresses[1] = ifltab[zdssKeys.kpathnameHash];
	addresses[2] = ifltab[zdssKeys.kaddTableHash];
	addresses[3] = fileHeader[zdssFileKeys.kbinSize];
	addresses[4] = ifltab[zdssKeys.khashTableBinAdd];
	if (getEndian()) {
		i8toi4(ifltab[zdssKeys.kbinPathLen], &pathnameSize, &numberChars);
	}
	else {
		i8toi4(ifltab[zdssKeys.kbinPathLen], &numberChars, &pathnameSize);
	}
	addresses[5] = (long long)zdssVals.infoSize + pathnameSize;
	addresses[6] = ifltab[zdssKeys.kaddInfoLastPath];

	if ((status != STATUS_RECORD_FOUND) && (status != STATUS_RECORD_NOT_FOUND)) {
		//  An error code
		return status;
	}

	if (status == STATUS_RECORD_FOUND) {
		info = (long long *)ifltab[zdssKeys.kinfo];
		//  Read in the info block
		i8toi4(ifltab[zdssKeys.kbinPathLen], &numberChars, &pathnameSize);
		numberInfo = zdssVals.infoSize + pathnameSize;
		istat = zget(ifltab, ifltab[zdssKeys.kaddInfoLastPath], (int *)info, numberInfo, 2);
		if (istat != 0) {
			return istat;
		}
		ifltab[zdssKeys.kinfoAddress] = ifltab[zdssKeys.kaddInfoLastPath];
		addresses[7] = info[zdssInfoKeys.kinfoValues1Number];
		addresses[8] = info[zdssInfoKeys.kinfoValues1Address];
		addresses[9] = info[zdssInfoKeys.kinfoAllocatedSize];

	}
	return status;
}

//   CALL zrecadd7(IFLTAB, CPATH, ILADD, ISTAT)
//  Fortran compatible interface

void zrecadd7_(long long *ifltab, const char* pathname, long long *addresses,
			 int *status, size_t lenPathname)
{
	char *path;

	path = stringFortToC(pathname, lenPathname);
	*status = zrecordAddresses(ifltab, path, addresses);
	free(path);
}

