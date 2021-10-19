#include <stdlib.h>
#include "heclib7.h"

#include "zdssKeys.h"
#include "zdssVals.h"
#include "zerrorCodes.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"

/**
*  Function:	zmemoryGet
*
*  Use:			Private (Internal)
*
*  Description:	Get or allocate memory to read/write the pathname bin.
*
*  Declaration: int zmemoryGet(long long *ifltab, int arrayLoc, int size, const char* memName);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				 int arrayLoc
*					The location in ifltab to store the address of the allocated memory
*
*				int size
*					How many long long words to allocate, separate of integrity keys
*
*				const char* memName
*					The name of the item being allocated, printed during debug or allocation failure
*
*
*	Returns:	STATUS_OKAY for successful operation.
*					errorCode for error, such as memory exhausted.  will return 0 for the memory address
*
*	Remarks:	Memory is allocated on first call and then only deallocated at the end of the program.
*					Keys are kept at the beginning and end of the array to ensure that memory has not been corrupted.
*					The pointer in ifltab points to the malloc'ed memory, not the beginning of the bin memory.
*					memory+0 points to a key to detect memory corruption
*					memory+1 points to the bin memory
*					memory+2+binSize points to the ending key
*					Normal use is just to return the pointer to memory for the pathname bin.
*
*	Called By:	zbinUpdate
*
*
*	Author:			Bill Charley
*	Date:			2013
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zmemoryGet(long long *ifltab, int arrayLoc, int size, const char* memName)
{

	long long *memory;


	if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zmemoryGet_ID, "Enter, type: ", memName);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zmemoryGet_ID, "Size: ", size);
	}

	memory = (long long*)calloc((size_t)(size+3), LONG_SIZE);
	if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_2) && memory != 0) {
		zmessageDebugLong(ifltab, DSS_FUNCTION_zmemoryGet_ID, "Memory allocated, location: ",(long long) memory);
	}
	if (memory == 0) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zmemoryGet_ID,
								zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, size, 0,
								zdssErrorSeverity.MEMORY_ERROR, "", memName);
	}

	memory[0] = DSS_MEMORY_INTEG_KEY;
	memory[1] = size;
	memory[size + 2] = DSS_MEMORY_INTEG_KEY;
	memory += 2L;
	//  Has to be kept like this - do not cast!
	ifltab[arrayLoc] = (long long)memory;

	if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugLong(ifltab, DSS_FUNCTION_zmemoryGet_ID, "Memory allocated at: ", ifltab[arrayLoc]);
	}

	return STATUS_OKAY;
}

