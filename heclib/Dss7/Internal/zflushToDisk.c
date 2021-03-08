#include <stdio.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"


/**
*  Function:	zflushToDisk
*
*  Use:			Private
*
*  Description:	Same as a commit; tells OS to be sure all writes are written to disk, not just in OS memory
*
*  Declaration: int zflushToDisk (long long *ifltab, int forceFlush);
*
*  Parameters:	long long ifltab
*					The integer file table array passed among DSS functions.
*
*				int forceFlush -  A boolean flag indicating to flush only if new writes, or flush regardless.
*					0: Flush only if new writes are shown
*					1: Flush regardless if no new writes.
*
*  Returns:		status of commit function, STATUS_OKAY if OK
*
*  Caution:		This can be extremely resource intensive for MS Windows!
*				Do not call unless it is an absolute requirement.
*
*  Note:		This function is usually called after a complete write while in multi-user access mode.
*				It is generally not called in exclusive access mode or multi-user advisory.
*				Does not flush internal write buffers; use zflushBuffers
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zflushToDisk (long long *ifltab, int forceFlush)
{

	int status;
	int ihandle;

	ihandle = (int)ifltab[zdssKeys.khandle];

	if (forceFlush == 0) {
		//  Have there been any writes that we need to flush?
		if (ifltab[zdssKeys.kwritesSinceFlush] == 0) {
			//  No new writes - just return
			if (zmessageLevel(ifltab, MESS_METHOD_PUT_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
				zmessageDebug(ifltab, DSS_FUNCTION_zflushToDisk_ID, "Flush bypassed because no new writes", "");
				zmessageDebugInt(ifltab, DSS_FUNCTION_zflushToDisk_ID, "zflushToDisk handle: ", ihandle);
			}
			return STATUS_OKAY;
		}
	}

	status = flushFile(ihandle);

	if (zmessageLevel(ifltab, MESS_METHOD_PUT_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zflushToDisk_ID, "zflushToDisk handle: ", ihandle);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zflushToDisk_ID, "zflushToDisk status: ", status);
	}

	if (status == STATUS_OKAY) {
		ifltab[zdssKeys.kwritesSinceFlush] = 0;
		ifltab[zdssKeys.kwritingNow] = 0;
		//flushFile(ihandle);
	}
	else {
		if (zmessageLevel(ifltab, MESS_METHOD_PUT_ID, MESS_LEVEL_GENERAL)) {
			zmessageDebugInt(ifltab, DSS_FUNCTION_zflushToDisk_ID, "Warning: Commit to disk failed for handle: ", ihandle);
			zmessageDebugInt(ifltab, DSS_FUNCTION_zflushToDisk_ID, "zflushToDisk status: ", status);
		}
	}
	return status;
}

