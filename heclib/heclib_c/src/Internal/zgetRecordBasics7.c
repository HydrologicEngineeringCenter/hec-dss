#include <string.h>
#include <stdio.h>

#include "heclib.h"
#include "zdssKeys.h"
#include "zStructRecordBasics.h"
#include "hecdssInternal.h"


/**
*  Function:	zgetRecordBasics7
*
*  Use:			Private;  use zgetRecordBasics instead
*
*  Description:	Function to get basic information about a single record, such as type, version, last written, etc
*
*  Declaration: int zgetRecordBasics7(long long *ifltab, zStructRecordBasics *recordBasics);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number.
*
*				zStructRecordBasics *recordbasics
*					A struct will contain information for a single record.
*					See zStructRecordBasics.h for description of zStructRecordBasics contents
*					This struct is created by the following method:
*						zStructRecordBasics* zstructRecordBasicsNew(const char* pathname);
*					When you are finished with the struct, the struct must be freed by a call to
*						void zstructFree(zStructRecordBasics *recordBasics)
*					NEVER REUSE A zStructRecordBasics, always free and create a new on.
*
*
*	Remarks:	zgetRecordBasics() is the public function.

*
*	See Also:	int zgetRecordSize (long long *ifltab, zStructRecordSize *recordSize);
*					int ztsGetSizes(long long *ifltab, zStructTimeSeries *tss,
*						zStructRecordSize *timeSeriesRecordSizes)
*					Which gives the combined sizes for a group of time series records
*					spanning the time window defined in zStructTimeSeries
*
*
*
*	Author:			Bill Charley
*	Date:			2017
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/

int zgetRecordBasics7(long long *ifltab, zStructRecordBasics *recordBasics)
{
	int status;
	char messageString[100];
	long long *info;
	long long *fileHeader;
	char *pathname;


	//  Check for correct DSS Version
	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zreadInfo_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
								zgetVersion(ifltab), 0, zdssErrorSeverity.WARNING, recordBasics->pathname, "");
	}

	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zreadInfo_ID, "Enter zgetRecordSize7, Pathname: ", recordBasics->pathname);
	}

	pathname = recordBasics->pathname;
	//  In case we just accessed this record, check to see if we have it in memory
	info = (long long *)ifltab[zdssKeys.kinfo];
	//  Double check that this is the correct path
	if (!zpathnameCompare(pathname, &ifltab[zdssKeys.kpathAddressInBin], strlen(pathname))) {
		//  No.  Read the info block
		ifltab[zdssKeys.kpathnameHash] = 0;
		status = zreadInfo(ifltab, pathname, 0);
		if (zisError(status)) {
			//  An error code
			status = zerrorUpdate(ifltab, status, DSS_FUNCTION_zreadInfo_ID);
			return status;
		}
		if (status != STATUS_RECORD_FOUND) {
			if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_USER_DIAG)) {
				_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, ZREAD_NOT_EXIST, zhandle(ifltab));
				zmessage2(ifltab, messageString, recordBasics->pathname);
			}
			return status;
		}
		info = (long long *)ifltab[zdssKeys.kinfo];
	}

	//  Fill in the struct
	i8toi4(info[zdssInfoKeys.kinfoTypeVersion], &recordBasics->recordType, &recordBasics->version);
	recordBasics->numberValues = (int)info[zdssInfoKeys.kinfoNumberData];
	recordBasics->logicalNumberValues = (int)info[zdssInfoKeys.kinfoLogicalNumber];
	recordBasics->values1Number = (int)info[zdssInfoKeys.kinfoValues1Number];
	recordBasics->values2Number = (int)info[zdssInfoKeys.kinfoValues2Number];
	recordBasics->values3Number = (int)info[zdssInfoKeys.kinfoValues3Number];

	recordBasics->internalHeaderNumber = (int)info[zdssInfoKeys.kinfoInternalHeadNumber];
	recordBasics->header2Number = (int)info[zdssInfoKeys.kinfoHeader2Number];
	recordBasics->values3Number = (int)info[zdssInfoKeys.kinfoValues3Number];
	recordBasics->userHeaderNumber = (int)info[zdssInfoKeys.kinfoUserHeadNumber];
	recordBasics->allocatedSize = (int)info[zdssInfoKeys.kinfoAllocatedSize];
	recordBasics->recLastWriteTimeMillis = info[zdssInfoKeys.kinfoLastWriteTime];
	recordBasics->recCreationTimeMillis = info[zdssInfoKeys.kinfoCreationTime];

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	recordBasics->fileLastWriteTimeMillis = fileHeader[zdssFileKeys.klastWriteTime];
	recordBasics->fileCreationTimeMillis = fileHeader[zdssFileKeys.kcreateDate];

	recordBasics->pathnameHash = ifltab[zdssKeys.kpathnameHash];
	recordBasics->tableHash = (int)ifltab[zdssKeys.ktableHash];


	if (zmessageLevel(ifltab, MESS_METHOD_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebugInt(ifltab, DSS_FUNCTION_zreadInfo_ID, "Exit zgetRecordBasics7, Status: ", status);
	}

	return status;
}

int printRecordBasics(long long *ifltab, char* pathname)
{
	int status;
	zStructRecordBasics recordBasics;


	recordBasics.pathname = pathname;
	status = zgetRecordBasics7(ifltab, &recordBasics);

	printf("\nRecord Basics for %s\n", pathname);

	if (status != STATUS_OKAY) {
		printf("*** Record not found or error ***\n\n");
		return status;
	}

	printf("\t recordType:\t%d\n", recordBasics.recordType);
	printf("\t version:\t%d\n", recordBasics.version);
	printf("\t numberValues:\t%d\n", recordBasics.numberValues);
	printf("\t logicalNumberValues:\t%d\n", recordBasics.logicalNumberValues);
	printf("\t values1Number:\t%d\n", recordBasics.values1Number);
	printf("\t values2Number:\t%d\n", recordBasics.values2Number);
	printf("\t values3Number:\t%d\n", recordBasics.values3Number);
	printf("\t internalHeaderNumber:\t%d\n", recordBasics.internalHeaderNumber);
	printf("\t header2Number:\t%d\n", recordBasics.header2Number);
	printf("\t userHeaderNumber:\t%d\n", recordBasics.userHeaderNumber);
	printf("\t allocatedSize:\t%d\n", recordBasics.allocatedSize);

	printf("\t recLastWriteTimeMillis:\t%lld\n", recordBasics.recLastWriteTimeMillis);
	printf("\t recCreationTimeMillis:\t%lld\n", recordBasics.recCreationTimeMillis);
	printf("\t fileLastWriteTimeMillis:\t%lld\n", recordBasics.fileLastWriteTimeMillis);
	printf("\t fileCreationTimeMillis:\t%lld\n", recordBasics.fileCreationTimeMillis);

	printf("\t tableHash:\t%d\n", recordBasics.tableHash);
	printf("\t pathnameHash:\t%lld\n", recordBasics.pathnameHash);

	return status;
}
