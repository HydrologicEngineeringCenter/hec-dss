
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "zdssMessages.h"
#include "zerrorCodes.h"
#include "hecdssInternal.h"

/**
*  Function:	zinitIfltab
*
*  Use:			Private (Internal)
*
*  Description:	Initializes the ifltab array for that file.  Only called once per file, and must be called when opened.
*
*  Called By:	zopenInternal
*
*  Declaration: void zinitIfltab (long long *ifltab)
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*
*	Returns:	int status
*					STATUS_OKAY
*					error code (probably not enough memory)
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zinitIfltab(long long *ifltab)
 {
	 int i;
	 int ifltabSize64 = 100;  //  ifltab must be dimensioned to int*4 200 or int*8 100
	 int defaultMultiUserLevel = 3;

	 //  Check for initialization
	 if (zdssVals.integrityKey != DSS_INTEGRITY_KEY) {
		zinit();
	 }

	 //   Clear the sys error loc.
#ifdef _MSC_VER
	 _set_errno(0);
#endif

	 //  Check for any zsetfi settings before we initialize ifltab
	 if (ifltab[zdssKeys.klocksDenied] == DSS_INTEGRITY_KEY) {   ///   FIX ME!!!!!!!!
		defaultMultiUserLevel = (int)ifltab[zdssKeys.kmultiUserAccess];
		ifltab[zdssKeys.klocksDenied] = 0;
	 }

	 for (i=0; i<ifltabSize64; i++) {
		ifltab[i] = 0;
	}
	 ifltab[zdssKeys.kwritingNow] = 0;
	 //  If the message information has not been set yet (it's "0"), set to default
	 if (ifltab[zdssKeys.kmessHandle] == 0) ifltab[zdssKeys.kmessHandle] = -1;  // Message handle
	 if (ifltab[zdssKeys.kmessLevel] == 0) ifltab[zdssKeys.kmessLevel] = -1;
	 if (ifltab[zdssKeys.kfortMessUnit] == 0) ifltab[zdssKeys.kfortMessUnit] = -1;
	 ifltab[zdssKeys.kfound] = 0;
	 ifltab[zdssKeys.kpathBin] = 0;  //  Explicit

	 ifltab[zdssKeys.kintegrityKey1] = DSS_INTEGRITY_KEY;
	 ifltab[zdssKeys.kintegrityKey2] = DSS_INTEGRITY_KEY;
	 ifltab[zdssKeys.kintegrityKey3] = DSS_INTEGRITY_KEY;

	 ifltab[zdssKeys.kfileWritten] = 0;

	 ifltab[zdssKeys.kmultiUserAccess] = defaultMultiUserLevel;

	 ifltab[zdssKeys.kiftPathHash] = 0;
	 //ifltab[zdssKeys.kreclaimLevel] = RECLAIM_ALL;
	 ///////////////////////////////////////////////////////////////////////////////
	 ifltab[zdssKeys.kreclaimLevel] = RECLAIM_NONE;
	 ///////////////////////////////////////////////////////////////////////////////


	 ifltab[zdssKeys.knumVersion] = 7;

	 //  0 (Zero) = LITTLE Endian (Windows/Intel, Linux/Intel)
	 //  1 (One)  = BIG Endian (Sun Solaris/RISC)
	 ifltab[zdssKeys.kswap] = getEndian();

	 /*
	 Now allocate space for the following areas in memory:
		1.  The file header (kfileHeader), also know as the permanent area
		2.  A pathname bin array (kpathBin) that will hold the current bin
		3.  A info array (kinfo), which will hold the current record header
		4.  The reclamation array (kreclaim)

		On both ends of all arrays, there is an integrity check key (DSS_ARRAY_INTEG_KEY),
		to verify that the memory area is intact and has not been written over.  The second
		word of the array is the size allocated (in long long), so the integrity check key
		can be verified
	 */


	 if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zinitIfltab_ID, "Completed ifltab Initialization", "");
	}

	 return STATUS_OKAY;
}

