#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "zerrorCodes.h"
#include "zdssMessages.h"


/**
*  Function:	zcheckKeys
*
*  Description:	Quick check to be sure that key locations in memory have not changed to ensure that memory has not become corrupt
*
*  Use:			Private (Internal)
*
*  Declaration: int zcheckKeys(long long *ifltab);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for invalid keys or corrupt memory
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zcheckKeys(long long *ifltab)
{
	int vers;
	int i;
	int size;
	int boolClosed;
	int errorCode;
	long long *address;


	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zcheckKeys_ID, "Enter", "");
	}

	errorCode = STATUS_OKAY;
	vers = zgetVersion(ifltab);
	if (vers != 7) {
		// uh oh... somethings wrong
		//  See if this is a version 6 ifltab
		if (vers == 6) {
			errorCode = zerrorProcessing(ifltab, DSS_FUNCTION_zcheckKeys_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
										 vers, 0, zdssErrorSeverity.WARNING_NO_FILE_ACCESS, "", "");
		}
		else if (vers < 0) {
			//  The file probably has never been opened?
			boolClosed = 1;
			for (i=0; i<20; i++) {
				if (ifltab[i] >= 0) {
					boolClosed = 0;
					break;
				}
			}
			if (boolClosed) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_zcheckKeys_ID, zdssErrorCodes.NOT_OPENED,
										0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
			}
		}

		//  Check to see if the file has been opened then closed (ifltab set to zero)
		boolClosed = 1;
		for (i=0; i<20; i++) {
			if (ifltab[i] != 0) {
				boolClosed = 0;
				break;
			}
		}
		if (boolClosed) {
			errorCode = zerrorProcessing(ifltab, DSS_FUNCTION_zcheckKeys_ID, zdssErrorCodes.CLOSED_FILE,
									0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
		}
		else {
			errorCode = zerrorProcessing(ifltab, DSS_FUNCTION_zcheckKeys_ID, zdssErrorCodes.IFLTAB_CORRUPT,
									0, 0, zdssErrorSeverity.MEMORY_ERROR, "", "");
		}

	}

	if ((errorCode == STATUS_OKAY) && (ifltab[zdssKeys.kintegrityKey1] != zdssVals.integrityKey)) {
		errorCode = zerrorProcessing(ifltab, DSS_FUNCTION_zcheckKeys_ID, zdssErrorCodes.KEY_CORRUPT,
										(int)ifltab[zdssKeys.kintegrityKey1], 0, zdssErrorSeverity.MEMORY_ERROR,
										"", "ifltab Key 1");
	}
	if ((errorCode == STATUS_OKAY) && (ifltab[zdssKeys.kintegrityKey2] != zdssVals.integrityKey)) {
		errorCode = zerrorProcessing(ifltab, DSS_FUNCTION_zcheckKeys_ID, zdssErrorCodes.KEY_CORRUPT,
										(int)ifltab[zdssKeys.kintegrityKey2], 0, zdssErrorSeverity.MEMORY_ERROR,
										"", "ifltab Key 2");
	}
	if ((errorCode == STATUS_OKAY) && (ifltab[zdssKeys.kintegrityKey3] != zdssVals.integrityKey)) {
		errorCode = zerrorProcessing(ifltab, DSS_FUNCTION_zcheckKeys_ID, zdssErrorCodes.KEY_CORRUPT,
										(int)ifltab[zdssKeys.kintegrityKey3], 0, zdssErrorSeverity.MEMORY_ERROR,
										"", "ifltab Key 3");
	}
	if ((errorCode == STATUS_OKAY) && (ifltab[zdssKeys.kfileHeader] != 0)) {
		address = (long long *)ifltab[zdssKeys.kfileHeader] - 2L;
		size = (int)address[1];
		if ((address[0] != DSS_MEMORY_INTEG_KEY) || (address[size+2] != DSS_MEMORY_INTEG_KEY)) {
			errorCode = zerrorProcessing(ifltab, DSS_FUNCTION_zcheckKeys_ID, zdssErrorCodes.KEY_CORRUPT,
										(int)address[0], address[size+2], zdssErrorSeverity.MEMORY_ERROR,
										"", "File header memory key");
		}
	}
	if ((errorCode == STATUS_OKAY) && (ifltab[zdssKeys.kpathBin] != 0)) {
		address = (long long *)ifltab[zdssKeys.kpathBin] - 2L;
		size = (int)address[1];
		if ((address[0] != DSS_MEMORY_INTEG_KEY) || (address[size+2] != DSS_MEMORY_INTEG_KEY)) {
			errorCode = zerrorProcessing(ifltab, DSS_FUNCTION_zcheckKeys_ID, zdssErrorCodes.KEY_CORRUPT,
										(int)address[0], address[size+2], zdssErrorSeverity.MEMORY_ERROR,
										"", "Pathname bin memory key");
		}
	}
	if ((errorCode == STATUS_OKAY) && (ifltab[zdssKeys.kinfo] != 0)) {
		address = (long long *)ifltab[zdssKeys.kinfo] - 2L;
		size = (int)address[1];
		if ((address[0] != DSS_MEMORY_INTEG_KEY) || (address[size+2] != DSS_MEMORY_INTEG_KEY)) {
			errorCode = zerrorProcessing(ifltab, DSS_FUNCTION_zcheckKeys_ID, zdssErrorCodes.KEY_CORRUPT,
										(int)address[0], address[size+2], zdssErrorSeverity.MEMORY_ERROR,
										"", "Record header memory (inf0) key");
		}
	}
	if ((errorCode == STATUS_OKAY) && (ifltab[zdssKeys.kreclaim] != 0)) {
		address = (long long *)ifltab[zdssKeys.kreclaim] - 2L;
		size = (int)address[1];
		if ((address[0] != DSS_MEMORY_INTEG_KEY) || (address[size+2] != DSS_MEMORY_INTEG_KEY)) {
			errorCode = zerrorProcessing(ifltab, DSS_FUNCTION_zcheckKeys_ID, zdssErrorCodes.KEY_CORRUPT,
										(int)address[0], address[size+2], zdssErrorSeverity.MEMORY_ERROR,
										"", "reclamation array memory key");
		}
	}

	if (errorCode != STATUS_OKAY) {
		ifltab[zdssKeys.kopenStatus] = OPEN_STAT_READ_ONLY;  //  Set read only on to prevent further damage
	}

	if (zmessageLevel(ifltab, MESS_METHOD_CHECK_ID, MESS_LEVEL_INTERNAL_DIAG_2)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcheckKeys_ID, "Exit, status: ", errorCode);
	}

	return errorCode;
}
