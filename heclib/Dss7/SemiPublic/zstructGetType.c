#include "hecdssInternal.h"
#include "zStructBasic.h"

/**
*  Function:	zstructGetType
*
*  Use:			Semi-private
*
*  Description:	 Returns the structure type for standard DSS data structs
*
*  Declaration: int zstructGetType(void *zstruct)
*
*  Parameters:
*				void *zstruct - One of the standard DSS data structs*
*
*  Returns:		structType:
*					0:  Invalid
*					1:  zStructTransfer
*					2:  zStructRecordSize
*					20 (DATA_TYPE_LOCATION): zStructLocation
*					100:  zStructTimeSeries
*					200:  zStructPairedData.h
*					300:  Text
*					310 (DATA_TYPE_TEXT_TABLE):  zStructTextTable.h
*
*  See Also:	zcompareDataSets()
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zstructGetType(void *zstruct)
{
	int structType;
	zStructBasic *zbstruct;

	zbstruct = (zStructBasic *)zstruct;
	structType = zbstruct->structType;

	return structType;
}

