#include <string.h>

#include "hecdssFort.h"
#include "hecdss7.h"
#include "hecdssInternal.h"
#include "zerrorCodes.h"

/*
*				int access:  read/write access to the file
*					0 - GENERAL_ACCESS:  Doesn't matter (no error if file doesn't have write permission)
*					1 - READ_ACCESS:  Read only (will not allow writing to file)
*					2 - MULTI_USER_ACCESS:  Read/Write permission with full mutil-user access
*						(usually slow, but necessary for multiple processes)
*					3 - SINGLE_USER_ADVISORY_ACCESS:  Read/Write permission with mutil-user advisory access
*						(throws an error if file is read only).  Best (and default access)
*					4 - EXCLUSIVE_ACCESS:  Exclusive write (used for squeezing).  Throws an error if not available.
 */
int zopenExtended(long long *ifltab, const char *dssFilename, int fileVersion,
			 int access, int maxExpectedPathnames, int hashSize, int binSize)
{
	int version;

	version = zgetFileVersion(dssFilename);

	if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zopen_ID, "Entering zopenExtended for file: ", dssFilename);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zopen_ID, "Dss file version ", version);
	 }

	return zopenInternal(ifltab, dssFilename, access, maxExpectedPathnames, hashSize, binSize, 0);
}

