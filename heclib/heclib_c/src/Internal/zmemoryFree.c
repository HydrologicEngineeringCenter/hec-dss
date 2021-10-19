#include <stdlib.h>
#include <stdio.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"

/**
*  Function:	zmemoryFree
*
*  Use:			Private (Internal)
*
*  Description:	Frees all memory allocated for this file.  memory is file (ifltab) based to ensure proper threading.
*					This function is called during close.  If zclose is not called, there will be a memory leak.
*
*  Declaration: void zmemoryFree(long long *ifltab);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*
*
*	Returns:	None
*
*	Remarks:		A memory leak will occur if this function is not called after access to a DSS file is completed.
*
*	Called By:	zcloseInternal
*
*
*	Author:			Bill Charley
*	Date:			2013
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void zmemoryFree(long long *ifltab)
{
	long long *memory;  //  (for compatibility with ifltab)
	int size;
	char messageString[80];

	//  Free the memory allocated for the pathname bin.
	memory = (long long *)ifltab[zdssKeys.kpathBin];
	//  Has the it already been freed?
	if (memory != 0) {
		//  No - first check to see if we've had corruption.
		//  This shouldn't cost anything.
		memory -= 2;
		size = (int)memory[1];
		if ((memory[0] != DSS_MEMORY_INTEG_KEY) || (memory[size+2] != DSS_MEMORY_INTEG_KEY)) {
			zerrorProcessing(ifltab, DSS_FUNCTION_zmemoryFree_ID, zdssErrorCodes.IFLTAB_CORRUPT,
									(int)memory[0], memory[size+2], zdssErrorSeverity.MEMORY_ERROR, "",
									"Invalid integrity flags for pathname bin memory...  Potential memory corruption");
		}
		//  Now free and mark as freed
		ifltab[zdssKeys.kpathBin] = 0;
		if (zmessageLevel(ifltab, MESS_METHOD_GENERAL_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;  Memory location: %llx",
				zhandle(ifltab), (long long)memory);
			 zmessageDebug(ifltab, DSS_FUNCTION_zmemoryFree_ID, "Free pathname bin memory;  Handle:", messageString);
		}
		free(memory);
	}

	//  Free the memory allocated for the record header (info area)
	memory = (long long *)ifltab[zdssKeys.kinfo];
	//  Has the it already been freed?
	if (memory != 0) {
		//  No - first check to see if we've had corruption.
		//  This shouldn't cost anything.
		memory -= 2;
		size = (int)memory[1];
		if ((memory[0] != DSS_MEMORY_INTEG_KEY) || (memory[size+2] != DSS_MEMORY_INTEG_KEY)) {
			zerrorProcessing(ifltab, DSS_FUNCTION_zmemoryFree_ID, zdssErrorCodes.IFLTAB_CORRUPT,
									(int)memory[0], memory[size+2], zdssErrorSeverity.MEMORY_ERROR, "",
									"Invalid integrity flags for record header memory...  Potential memory corruption");
		}
		//  Now free and mark as freed
		ifltab[zdssKeys.kinfo] = 0;
		if (zmessageLevel(ifltab, MESS_METHOD_GENERAL_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;  Memory location: %llx",
				zhandle(ifltab), (long long)memory);
			 zmessageDebug(ifltab, DSS_FUNCTION_zmemoryFree_ID, "Free record header memory;  Handle:", messageString);
		}
		free(memory);
	}

	//  Free the memory allocated for the reclaim array
	memory = (long long *)ifltab[zdssKeys.kreclaim];
	//  Has the it already been freed?
	if (memory != 0) {
		//  No - first check to see if we've had corruption.
		//  This shouldn't cost anything.
		memory -= 2;
		size = (int)memory[1];
		if ((memory[0] != DSS_MEMORY_INTEG_KEY) || (memory[size+2] != DSS_MEMORY_INTEG_KEY)) {
			zerrorProcessing(ifltab, DSS_FUNCTION_zmemoryFree_ID, zdssErrorCodes.IFLTAB_CORRUPT,
									(int)memory[0], memory[size+2], zdssErrorSeverity.MEMORY_ERROR, "",
									"Invalid integrity flags for reclaim array memory...  Potential memory corruption");
		}
		//  Now free and mark as freed
		ifltab[zdssKeys.kreclaim] = 0;
		if (zmessageLevel(ifltab, MESS_METHOD_GENERAL_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;  Memory location: %llx",
				zhandle(ifltab), (long long)memory);
			 zmessageDebug(ifltab, DSS_FUNCTION_zmemoryFree_ID, "Free reclaim array memory;  Handle:", messageString);
		}
		free(memory);
	}

	//  Free the memory allocated for the main file header
	memory = (long long *)ifltab[zdssKeys.kfileHeader];
	//  Has the it already been freed?
	if (memory != 0) {
		//  No - first check to see if we've had corruption.
		//  This shouldn't cost anything.
		memory -= 2;
		size = (int)memory[1];
		if ((memory[0] != DSS_MEMORY_INTEG_KEY) || (memory[size+2] != DSS_MEMORY_INTEG_KEY)) {
			zerrorProcessing(ifltab, DSS_FUNCTION_zmemoryFree_ID, zdssErrorCodes.IFLTAB_CORRUPT,
									(int)memory[0], memory[size+2], zdssErrorSeverity.MEMORY_ERROR, "",
									"Invalid integrity flags for main file header...  Potential memory corruption");
		}
		//  Now free and mark as freed
		ifltab[zdssKeys.kfileHeader] = 0;
		if (zmessageLevel(ifltab, MESS_METHOD_GENERAL_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
			_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, " %d;  Memory location: %llx",
				zhandle(ifltab), (long long)memory);
			 zmessageDebug(ifltab, DSS_FUNCTION_zmemoryFree_ID, "Free main file header memory;  Handle:", messageString);
		}
		free(memory);
	}

	if (ifltab[zdssKeys.kcatStruct]) {
		zstructFree((zStructCatalog*)ifltab[zdssKeys.kcatStruct]);
		ifltab[zdssKeys.kcatStruct] = 0;
	}

	if (ifltab[zdssKeys.kCRCtable]) {
		free((void *)ifltab[zdssKeys.kCRCtable]);
		ifltab[zdssKeys.kCRCtable] = 0;
	}

	if (ifltab[zdssKeys.kfilename]) {
		free((void *)ifltab[zdssKeys.kfilename]);
		ifltab[zdssKeys.kfilename] = 0;
	}
	if (ifltab[zdssKeys.kfullFilename]) {
		free((void *)ifltab[zdssKeys.kfullFilename]);
		ifltab[zdssKeys.kfullFilename] = 0;
	}

}

