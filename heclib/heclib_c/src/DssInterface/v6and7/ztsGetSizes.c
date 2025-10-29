#include "heclib.h"


/**
*  Function:	ztsGetSizes
*
*  Use:			Public
*
*  Description:	Gets size information about a time series record or data set (series of records) blocks
*
*  Declaration: int ztsGetSizes(long long *ifltab, zStructTimeSeries *tss, zStructRecordSize *recordSize);
*
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				const char* pathname
*					The pathname of the record or data set to get sizes from.
*
*				zStructTimeSeries *tss
*					A pointer to a time series struct that identifies the start and end dates/times
*					of the blocks.  To retrieve the sizes for a single record only, set this to zero (void *)0.
*					Note:  This reports sizes for blocks defined by the time window, not just the time window.
*
*				zStructRecordSize *recordSize
*					The time series record sizes struct that will be returned with the size of either the
*					single record (ztsTimeWindow set to 0), or the accumulation of records using the dates/times
*					identified in ztsTimeWindow.
*
*
*	Returns:	int status
*					STATUS_OKAY
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*
*	Remarks:	Time Series may be either regular or irregular.
*					Reports sizes for blocks, not within blocks.
*					Set time window to null for single record and read info before use!
*					Assumes correct data types, pathname, etc.  Does minimal error checking -
*					it is assumed that error checking is done prior to this function being called.
*					If an error occurs in a lower function, just pass that error code back
*					(don't update at this level, as the calling function will.)
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int ztsGetSizes(long long *ifltab, zStructTimeSeries *tss, zStructRecordSize *timeSeriesRecordSizes)
{

	//  get sizes internal returns number in block, not number
	//  in time window.  Report back number in time window, if set
	int	status = ztsGetSizesInternal (ifltab, tss->timeWindow,  timeSeriesRecordSizes);

	if (status >= 0) {
		return STATUS_OKAY;
	}
	return status;
}



