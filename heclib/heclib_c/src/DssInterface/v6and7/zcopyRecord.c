#include <stdlib.h>
#include <string.h>

#include "heclib.h"
#include "hecdssInternal.h"
#include "zdssKeys.h"
#include "verticalDatum.h"


/**
*  Function:	zcopyRecord
*
*  Use:			Public
*
*  Description:	Copy or duplicate a record
*
*  Declaration: int zcopyRecord(long long *ifltabFrom, long long *ifltabTo, const char *pathnameFrom, const char *pathnameTo);
*
*  Parameters:
*				long long ifltabFrom
*					The ifltabFrom of the DSS file being copied from.  Maybe either version 6 or 7.
*
*				long long *ifltabTo
*					The ifltabFrom of the DSS file being copied to.  This can either be a version 6 or 7 file.
*
*				const char *pathnameFrom
*					The pathname to copy.  Must be a full valid pathname; a single record, not a dataset
*
*				const char *pathnameTo
*					The pathname to copy to.  Often the same as pathnameFrom, but if duplicating, must be a different name
*
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode for error
*					errorCode may be decoded by function zerrorDecode()
*
*
*	Remarks:	This function only direct the call to the right function based on file versions.
*
*
*	Author:			Bill Charley
*	Date:			2010
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zcopyRecord(long long *ifltabFrom, long long *ifltabTo, const char *pathnameFrom, const char *pathnameTo)
{
	int versFileFrom;
	int versFileTo;
	int status;
	int dataType;
	int boolInternalCopy;
	int positions[7];
	int lenPartE;
	int ipos;
	int lenPartsFrom;
	int startFrom;
	int lenPartsTo;
	int vdiOverwrite = 0;
	int elevCopy = 0;
	size_t pathnameFromLen;
	size_t pathnameToLen;
	zStructTimeSeries *tss;
	zStructPairedData *pds;


	if (zmessageLevel(ifltabFrom, MESS_METHOD_COPY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID, "Enter.  Pathname from: ", pathnameFrom);
		zmessageDebugInt(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID, "Handle from: ", zhandle(ifltabFrom));
		zmessageDebug(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID, "Pathname to: ", pathnameTo);
		zmessageDebugInt(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID, "Handle to: ", zhandle(ifltabTo));
	}

	if (!pathnameFrom) {
		return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "pathnameFrom is null");
	}
	if (!pathnameTo) {
		return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "pathnameTo is null");
	}

	pathnameFromLen = strlen(pathnameFrom);
	pathnameToLen = strlen(pathnameTo);

	versFileFrom = zgetVersion(ifltabFrom);
	versFileTo = zgetVersion(ifltabTo);
	status = 0;

	elevCopy = pathnameIsElevTsOrPd(pathnameFrom) && pathnameIsElevTsOrPd(pathnameTo);

	dataType = zdataType(ifltabFrom, pathnameFrom);
	if (dataType < 0) {
		return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID,
			zdssErrorCodes.RECORD_DOES_NOT_EXIST, 0,
			0, zdssErrorSeverity.WARNING, pathnameFrom, "");
	}

	if ((versFileFrom == 7) && (versFileTo == 7)) {
		//  Be sure both paths are okay with vers 6 and 7 differences
		boolInternalCopy = 1;
		if ((dataType >= DATA_TYPE_RTS) && (dataType < DATA_TYPE_PD)) {
			//  An E part of "MIN" is not compatible with "Minutes" for version 7
			zpathnamePartPositions(pathnameTo, pathnameToLen, positions, 7);
			lenPartE = positions[5] - positions[4] - 1;
			if (lenPartE < 6) {
				ipos = positions[5] - 4;
				if (strncmp(&pathnameTo[ipos], "MIN/", 4) == 0) {
					boolInternalCopy = 0;
				}
			}
			//  D and E parts must be identical for time series data to use internal copy
			//  If not, just do the long way (read and write) instead of internal copy
			//  Long way should take care of any issues
			if (boolInternalCopy) {
				lenPartsFrom = positions[5] - positions[3];
				startFrom = positions[3];
				zpathnamePartPositions(pathnameFrom, strlen(pathnameFrom), positions, 7);
				lenPartsTo = positions[5] - positions[3];
				if (lenPartsFrom != lenPartsTo) {
					boolInternalCopy = 0;
				}
				else {
					if (strncmp(&pathnameFrom[startFrom], &pathnameTo[positions[3]], lenPartsFrom) != 0) {
						boolInternalCopy = 0;
					}
				}
			}
			if (boolInternalCopy && elevCopy) {
				//----------------------------------------------------------------------------//
				// force standard copy if source and destination records are Elev time series //
				//----------------------------------------------------------------------------//
				boolInternalCopy = 0; // zcopyRecordInternal doesn't copy VDI in location record
			}
		}
		else if ((dataType >= DATA_TYPE_PD) && (dataType < DATA_TYPE_TEXT)) {
			if (pathnameIsElevPd(pathnameFrom) && pathnameIsElevPd(pathnameTo)) {
				boolInternalCopy = 0; // zcopyRecordInternal doesn't copy VDI in location record
			}
		}
	}
	else {
		boolInternalCopy = 0;
	}

	if (boolInternalCopy) {
		status = zreadInfo(ifltabFrom, pathnameFrom, 0);
		if (status == 0) {
			status = zcopyRecordInternal(ifltabFrom, ifltabTo, pathnameTo, 0);
		}
	}
	else {

		//  Try to copy using standard data types
		if ((dataType >= DATA_TYPE_RTS) && (dataType < DATA_TYPE_PD)) {
			tss = zstructTsNew(pathnameFrom);
			if (!tss) {
				return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
										zdssErrorSeverity.MEMORY_ERROR,
										pathnameFrom, "Allocating time series struct in zcopyRecord");
			}
			status = ztsRetrieve(ifltabFrom, tss, -2, 0, 1);
			if (zisError(status)) {
				zstructFree(tss);
				return zerrorUpdate(ifltabFrom, status, DSS_FUNCTION_zcopyRecord_ID);
			}
			//  Change the struct pathname to the new one
			free(tss->pathname);
			tss->pathname = mallocAndCopyPath(pathnameTo);
			if (!tss->pathname) {
				zstructFree(tss);
				return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
										zdssErrorSeverity.MEMORY_ERROR,
										pathnameTo, "Allocating ts pathname");
			}
			if (tss->pathnameInternal) {
				free(tss->pathnameInternal);
				tss->pathnameInternal = 0;
			}
			if (tss->timeWindow) {
				free(tss->timeWindow);
				tss->timeWindow = 0;
			}
			if (tss->locationStruct) {
				if (tss->locationStruct->pathname) {
					free(tss->locationStruct->pathname);
				}
				tss->locationStruct->pathname = mallocAndCopyPath(pathnameTo);
				if (!tss->pathname) {
					zstructFree(tss);
					return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID,
						zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
						zdssErrorSeverity.MEMORY_ERROR,
						pathnameTo, "Allocating ts location pathname");
				}
			}
			if (versFileTo == 6 && elevCopy) {
				//-----------------------------------//
				// always allow record copy to DSS 6 //
				//-----------------------------------//
				zquery("VDOW", "", 0, &vdiOverwrite);
				zset("VDOW", "", 1);
			}
			status = ztsStore(ifltabTo, tss, 0);
			if (versFileTo == 6) {
				zset("VDOW", "", vdiOverwrite);
			}
			zstructFree(tss);
			if (zisError(status)) {
				return zerrorUpdate(ifltabTo, status, DSS_FUNCTION_zcopyRecord_ID);
			}
		}
		else if ((dataType >= DATA_TYPE_PD) && (dataType < DATA_TYPE_TEXT)) {
			pds = zstructPdNew(pathnameFrom);
			if (!pds) {
				return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
										zdssErrorSeverity.MEMORY_ERROR,
										pathnameFrom, "Allocating paired data struct in zcopyRecord");
			}
			status = zpdRetrieve(ifltabFrom, pds, 0);
			if (zisError(status)) {
				zstructFree(pds);
				return zerrorUpdate(ifltabFrom, status, DSS_FUNCTION_zcopyRecord_ID);
			}
			//  Change the struct pathname to the new one
			free(pds->pathname);
			pds->pathname = mallocAndCopyPath(pathnameTo);
			if (!pds->pathname) {
				zstructFree(pds);
				return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID,
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
										zdssErrorSeverity.MEMORY_ERROR,
										pathnameTo, "Allocating paired data pathname");
			}
			if (pds->locationStruct) {
				if (pds->locationStruct->pathname) {
					free(pds->locationStruct->pathname);
				}
				pds->locationStruct->pathname = mallocAndCopyPath(pathnameTo);
				if (!pds->pathname) {
					zstructFree(pds);
					return zerrorProcessing(ifltabFrom, DSS_FUNCTION_zcopyRecord_ID,
						zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
						zdssErrorSeverity.MEMORY_ERROR,
						pathnameTo, "Allocating paired data location pathname");
				}
			}
			if (versFileTo == 6 && elevCopy) {
				//-----------------------------------//
				// always allow record copy to DSS 6 //
				//-----------------------------------//
				zquery("VDOW", "", 0, &vdiOverwrite);
				zset("VDOW", "", 1);
			}
			status = zpdStore(ifltabTo, pds, 0);
			if (versFileTo == 6) {
				zset("VDOW", "", vdiOverwrite);
			}
			zstructFree(pds);
			if (zisError(status)) {
				return zerrorUpdate(ifltabTo, status, DSS_FUNCTION_zcopyRecord_ID);
			}
		}
		//  Text struct not operational yet for dss-6
		else {
			//  All others
			zcopyrecord6_(ifltabFrom, ifltabTo, pathnameFrom, pathnameTo, &status, pathnameFromLen, pathnameToLen);
		}
	}
	return status;
}

void zcopyrecord_ (long long *ifltabFrom, long long *ifltabTo, const char *pathnameFrom, const char *pathnameTo, int *status,
					size_t pathnameFromLen, size_t pathnameToLen)
{
	char *pathFrom;
	char *pathTo;

	pathFrom = stringFortToC(pathnameFrom, pathnameFromLen);
	pathTo = stringFortToC(pathnameTo, pathnameToLen);

	*status = zcopyRecord(ifltabFrom, ifltabTo, pathFrom, pathTo);

	free(pathFrom);
	free(pathTo);
}

