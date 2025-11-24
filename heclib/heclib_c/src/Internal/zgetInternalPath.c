#include <stdlib.h>
#include <string.h>

#include "heclib.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"

/**
*  Function:	zgetInternalPath
*
*  Use:			Private
*
*  Description:	 Returns the internal pathname of the struct, or the public one if no internal
*
*  Declaration: char* zgetInternalPath(void *zstruct);
*
*  Parameters:
*				void *zstruct - One of the standard DSS data structs
*
*  	structType:
*					0:  Invalid
*					1:  zStructTransfer
*					2:  zStructRecordSize
*
*					20 (DATA_TYPE_LOCATION): zStructLocation
*					100:  zStructTimeSeries
*					200:  zStructPairedData.h
*					300:  Text
*					310 (DATA_TYPE_TEXT_TABLE):  zStructTextTable.h
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


char *zgetInternalPathTs(zStructTimeSeries *zstruct)
{
	if (zstruct->pathnameInternal) {
		return zstruct->pathnameInternal;
	}
	return zstruct->pathname;
}
char *zgetInternalPathTr(zStructTransfer *zstruct)
{
	return zstruct->pathname;
}
char *zgetInternalPathRs(zStructRecordSize *zstruct)
{
	return zstruct->pathname;
}
char *zgetInternalPathRb(zStructRecordBasics *zstruct)
{
	return zstruct->pathname;
}

char *zgetInternalPathLo(zStructLocation *zstruct)
{
	return zstruct->pathname;
}
char *zgetInternalPathPd(zStructPairedData *zstruct)
{
	return zstruct->pathname;
}
char *zgetInternalPathTx(zStructText *zstruct)
{
	return zstruct->pathname;
}



char *zgetInternalPath(void *zstruct)
{
	int structType;

	structType = zstructGetType(zstruct);

	if (structType == STRUCT_TYPE_TRANSFER) {
		return zgetInternalPathTr((zStructTransfer *)zstruct);
	}
	else if (structType == STRUCT_TYPE_RECORD_SIZES) {
		return zgetInternalPathRs((zStructRecordSize *)zstruct);
	}
	else if (structType == STRUCT_TYPE_RECORD_BASICS) {
		return zgetInternalPathRb((zStructRecordBasics *)zstruct);
	}
	else if (structType == STRUCT_TYPE_RECORD_ADDRESSES) {
		//
	}
	else if (structType == DATA_TYPE_LOCATION) {
		return zgetInternalPathLo((zStructLocation *)zstruct);
	}
	else if (structType == DATA_TYPE_RTS) {
		return zgetInternalPathTs((zStructTimeSeries *)zstruct);
	}
	else if (structType == DATA_TYPE_PD) {
		return zgetInternalPathPd((zStructPairedData *)zstruct);
	}
	else if (structType == DATA_TYPE_TEXT_TABLE) {
		return zgetInternalPathTx((zStructText *)zstruct);
	}
	return 0;
}




