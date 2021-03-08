#include <stdlib.h>

#include "heclib7.h"
#include "hecdssInternal.h"
#include "zStructPairedData.h"

/**
*  Function:	zstructPdNewFloats
*
*  Use:			Public
*
*  Description:	Creates a new paired data struct with data for storing a paired data float data set.
*
*  Declaration: zStructPairedData* zstructPdNewFloats(const char* pathname, float *floatOrdinates, float *floatValues,
*													  int numberOrdinates, int numberCurves,
*													  const char *unitsIndependent, const char *typeIndependent,
*													  const char *unitsDependent, const char *typeDependent);
*
*  Parameters:	const char* pathname
*					The pathname of the record to store.  The pathname must be exact, but is case insensitive
*					(i.e., "Apart" is the same as "APART"), and have a correct C (independent-dependent name) part.
*					For example, "/Stage-Damage/", where stage is the independent parameter and Damage is the dependent
*					You can only one stage array (column), but you can have multiple damage arrays
*					(e.g., "Residential", "Commercial", "Ag", "Total")
*					 A copy of the pathname is used in struct.
*
*				float *floatOrdinates
*					The ordiante array containing the data to store.  Must be numberOrdinates long
*
*				float *floatValues
*					The curves array containing the data to store.  Must be numberCurves * numberOrdinates long.
*					The nominal dimension is floatValues[numberCurves][numberOrdinates]
*					Each curve (column) must follow the previous column in memory
*
*				int numberOrdinates
*					The number of ordinates (rows) in the dataset.
*
*				int numberCurves
*					The number of curves (columns) in the dataset (not not include the ordinate column.)
*
*				const char *unitsIndependent
*					The units of the independent data, such as "Feet".
*					The number of characters in units is not limited, but generally should not be greater than 25.
*
*				const char *typeIndependent
*					The type of the independent data.  Valid values include "Linear", "Percent", "Probability".
*					The number of characters in units is not limited, but generally should not be greater than 25.
*
*				const char *unitsDependent
*					The units of the dependent data, such as "Dollars".
*
*				const char *typeDependent
*					The type of the dependent data.
*
*
*
*	Returns:	zStructPairedData*
*					An address to the paired struct created ready to be stored with zpdStore().
*					Additional data, such as quality values or notes can be added.
*
*   Note:		ALWAYS call function zstructFree(struct) when finished using.
*
*	Remarks:	This is an extension of zstructPdNew().
*					For STORING data only
*					(To retrieve data, use zstructPdNew)
*					The arrays are used directly (not copied), so they must stay intact until the store is complete.
*					You must not reuse this struct.  Make a new one for every dataset.
*
*	See:		zpdStore for use and definition of zStructPairedData
*
*	See Also:	zstructPdNew()
*				zstructPDsNewDoubles()
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


zStructPairedData* zstructPdNewFloats(const char* pathname, float *floatOrdinates, float *floatValues,
									  int numberOrdinates, int numberCurves,
									  const char *unitsIndependent, const char *typeIndependent,
									  const char *unitsDependent, const char *typeDependent)
{
	zStructPairedData *pds;

	pds = zstructPdNew(pathname);

	pds->floatOrdinates = floatOrdinates;
	pds->floatValues = floatValues;
	pds->numberOrdinates = numberOrdinates;
	pds->numberCurves = numberCurves;

	if (unitsIndependent) {
		pds->unitsIndependent = mallocAndCopy(unitsIndependent);
		pds->allocated[zSTRUCT_unitsIndependent] = 1;
	}
	if (unitsIndependent) {
		pds->typeIndependent = mallocAndCopy(typeIndependent);
		pds->allocated[zSTRUCT_typeIndependent] = 1;
	}
	if (unitsIndependent) {
		pds->unitsDependent = mallocAndCopy(unitsDependent);
		pds->allocated[zSTRUCT_unitsDependent] = 1;
	}
	if (unitsIndependent) {
		pds->typeDependent = mallocAndCopy(typeDependent);
		pds->allocated[zSTRUCT_typeDependent] = 1;
	}

	return pds;
}

