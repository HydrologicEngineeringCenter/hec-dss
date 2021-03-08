#include <string.h>

#include "heclib.h"
#include "hecdssInternal.h"

#include "zStructTimeSeries.h"


/**
*  Function:	zstructTsNewIrregDoubles
*
*  Use:			Public
*
*  Description:	Creates a new time series struct with data for storing a irregular-interval double data set.
*					Note:  For STORING data only.
*
*  Declaration: zstructTsNewIrregDoubles(const char* pathname, double *doubleValues, int numberValues,
*										 int *itimes, int timeGranularitySeconds, const char* startDateBase,
*										 const char *units, const char *type);
*
*  Parameters:	const char* pathname
*					The pathname of the record to store.  The pathname must be exact, but is case insensitive
*					(i.e., "Apart" is the same as "APART"), and have a correct E (block or pseudo-interval) part
*					 A copy of the pathname is used in struct.
*
*				double *doubleValues
*					The double array containing the data to store.
*
*				int numberValues
*					The number of data values in array doubleValues and itimes to store.
*
*				int *itimes
*					An integer array of minutes or seconds that correspond to the date/time for each value.
*					If startDateBase is null or "", then these are minutes or seconds from 01 Jan 1900;
*					otherwise this array is to contain minutes/seconds from startDateBase (start of the day for that date.)
*					itimes must be the numberValues long and must have a one-to-one correspondence with doubleValues.
*
*				int timeGranularitySeconds
*					The number of seconds a unit in *itimes represents, usually
*					MINUTE_GRANULARITY (60) or SECOND_GRANULARITY (1)
*
*				const char *startDateBase (optional)
*					If the time array starts from any other date than 01Jan1900, then that start date is given here.
*					That date is added to each element in itimes to obtain the correct date.
*					startDateBase is usually used when times in itimes might over run and integer words, such
*					as when seconds are used as the granularity or times are very large.
*					The date may appear similar to "23MAR1985".  Multiple date formats are supported.
*					If the default base date is used (01 Jan 1900), set to null or "".
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
*				zstructTsNewRegFloats()
*				zstructTsNewIrregFloats()
*				zstructTsNewRegDoubles()
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

zStructTimeSeries* zstructTsNewIrregDoubles(const char* pathname, double *doubleValues, int numberValues,
											int *itimes, int timeGranularitySeconds, const char* startDateBase,
											const char *units, const char *type)
{
	zStructTimeSeries *tss;


	tss = zstructTsNew(pathname);

	if ((startDateBase) && (strlen(startDateBase) > 4)) {
		tss->julianBaseDate = dateToJulian(startDateBase);
	}
	else {
		tss->julianBaseDate = 0;
	}

	tss->numberValues = numberValues;
	tss->times = itimes;
	tss->timeGranularitySeconds = timeGranularitySeconds;
	tss->doubleValues = doubleValues;

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

