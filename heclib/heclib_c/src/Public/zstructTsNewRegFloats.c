#include <stdlib.h>
#include <string.h>

#include "hecdss7.h"

/**
*  Function:	zstructTsNewRegFloats
*
*  Use:			Public
*
*  Description:	Creates a new time series struct with data for storing a regular-interval float data set.
*
*  Declaration: zStructTimeSeries* zstructTsNewRegFloats(const char* pathname, float *floatValues, int numberValues,
*														 const char *startDate, const char *startTime,
*														 const char *units, const char *type);
*
*  Parameters:	const char* pathname
*					The pathname of the record to store.  The pathname must be exact, but is case insensitive
*					(i.e., "Apart" is the same as "APART"), and have a correct E (interval) part
*					 A copy of the pathname is used in struct.
*
*				float *floatValues
*					The float array containing the data to store.
*
*				int numberValues
*					The number of data values in array floatValues to store.
*
*				const char *startDate
*					The date of the first data value, such as "23MAR1985".  Multiple date formats are supported.
*
*				const char *startTime
*					The time of the first data value, such as "0700" or "07:00:00".
*
*				const char *units
*					The units of the data, such as "CFS".
*					The number of characters in units is not limited, but generally should not be greater than 25.
*
*				const char *type
*					The type of data.  Valid values include "PER-AVER", "PER-CUM", "INST-VAL", and "INST-CUM".
*					The number of characters in units is not limited, but generally should not be greater than 25.
*
*
*
*	Returns:	zStructTimeSeries*
*					An address to the time series struct created ready to be stored with ztsStore().
*					Additional data, such as quality values or notes can be added.
*
*   Note:		ALWAYS call function zstructFree(struct) when finished using.
*
*	Remarks:	This is an extension of zstructTsNew().
*					For STORING data only
*					(To retrieve data, use zstructTsNew and provide time window in call to ztsRetrieve.)
*					The arrays are used directly (not copied), so they must stay intact until the store is complete.
*					You must not reuse this struct.  Make a new one for every dataset.
*
*	See:		ztsStore for use and definition of zStructTimeSeries
*
*	See Also:	zstructTsNew()
*				zstructTsNewRegDoubles()
*				zstructTsNewIrregFloats()
*				zstructTsNewIrregDoubles()
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


zStructTimeSeries* zstructTsNewRegFloats(const char* pathname, float *floatValues, int numberValues,
										 const char *startDate, const char *startTime, const char *units, const char *type)
{
	zStructTimeSeries *tss;


	tss = zstructTsNew(pathname);

	if (startDate && (strlen(startDate) > 0)) {
		tss->startJulianDate = dateToJulian(startDate);
	}
	if (startTime && (strlen(startTime) > 0)) {
		tss->startTimeSeconds = timeStringToSeconds(startTime);
	}

	tss->floatValues = floatValues;
	tss->numberValues = numberValues;

	if (units) {
		tss->units = mallocAndCopy(units);
		tss->allocated[zSTRUCT_TS_units] = 1;
	}
	if (type) {
		tss->type = mallocAndCopy(type);
		tss->allocated[zSTRUCT_TS_type] = 1;
	}

	return tss;
}

