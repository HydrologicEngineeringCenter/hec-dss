#include "zdssKeys.h"
#include "zdssVals.h"
#include "hecdss7.h"
#include "zerrorCodes.h"


/**
*  Function:	zerror
*
*  Use:			Public
*
*  Description:	Quick functions to determine if an error has occurred
*
*  Declaration: int zerrorCheck();
*						returns 0 if no error has occurred.
*						returns severity (> 0) for the last error
*
*				int zfileError(long long *ifltab);
*						returns 0 if no error has occurred.
*						returns severity (> 0) for the most severe error that has happened
*				Severity:
*							INFORMATION		1	An inconsequential action failed
*							WARNING			2	Unable to compete the request given
*							INVALID_ARGUMENT 3	A invalid argument (e.g., a null pointer) was passed to a function
*							WARNING_NO_WRITE_ACCESS	4	You cannot write to the file
*							WARNING_NO_FILE_ACCESS	5	You cannot read or write to the file
*							WRITE_ERROR		6	An error was thrown on a write action.
*							READ_ERROR		7	An error was thrown on a read action.  The file maybe damaged.  It might be recovered by a squeeze, at the user's discretion.
*							CORRUPT_FILE	8	Flags and pointers are not correct, indicating that something has changed the file outside of DSS
*							MEMORY_ERROR	9	Flags and pointers in memory are not correct
*
*					int zerrorCode(long long *ifltab);
*							returns 0 if no error has occurred.
*							returns the actual error code, if an error (will be a large negative number).
*								This code can be deciphered using zerrorDecode
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/



int zfileError(long long *ifltab)
{
	return (int)ifltab[zdssKeys.kerrorCondition];
}

int zerrorCode(long long *ifltab)
{
	return (int)ifltab[zdssKeys.kerrorCode];
}




int zfileerror_(long long *ifltab)
{
	return (int)ifltab[zdssKeys.kerrorCondition];
}

void zerrorcode_(long long *ifltab, int *errorCode)
{
	*errorCode = zerrorCode(ifltab);
}

