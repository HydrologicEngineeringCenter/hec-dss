#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"
#include "hecdssInternal.h"
#include "zStructTsTimeWindow.h"
#include "fortran_string_len_size.h"



//  FORTRAN Callable
//  call zsits7(ifltab1, cpath1, itimes, values, nvals, julianBase, 'CFS', 'PER-AVER', 0, status)
//  For compatibility purposes.   function is 
//  for storing only float values

void zsits7_(long long *ifltab, const char *path, 				
				int *timeArray, int *values, 
				int *numberValues, int *julianBase,
				const char *units, const char *type, 
				int *storageFlag, int *status,
				slen_t pathLen, slen_t unitsLen, slen_t typeLen)
{
	char *pathname;
	char *cunits;
	char *ctype;
	char *cnull = 0;
	zStructTimeSeries *tss;
	
	pathname = stringFortToC(path, pathLen);
	cunits = stringFortToC(units, unitsLen);
	ctype = stringFortToC(type, typeLen);	

	tss = zstructTsNewIrregFloats(pathname, (float *)values, *numberValues, 
								  timeArray, MINUTE_GRANULARITY, cnull, cunits, ctype);
	tss->julianBaseDate = *julianBase;
	*status = ztsStore(ifltab, tss, *storageFlag);

	free(pathname);
	free(cunits);
	free(ctype);
	zstructFree(tss);

}



//  timeGranularitySeconds:  The number of seconds a unit in *timeArray represents, usually
//					MINUTE_GRANULARITY (60) or SECOND_GRANULARITY (1)

void zsitsi7_(long long *ifltab, const char *path,
			  int *timeArray, float *singleValues, double *doubleValues,
			  int *boolDoubles, int *numberValues, int *julianBaseDate,
			  int *quality, int *boolQuality, 
			  const char *units, const char *type,
			  int *userHeader, int *userHeaderNumber, 
			  double coordinates[], int *numberCoordinates,
			  int coordinateDescription[],  int *numbCoordDescription,
			  int *storageFlag, int *status,
			  slen_t pathLen, slen_t unitsLen, slen_t typeLen)
{
	char *pathname;
	char *cunits;
	char *ctype;
	char *cnull = 0;
	zStructTimeSeries *tss;
	
	pathname = stringFortToC(path, pathLen);
	cunits = stringFortToC(units, unitsLen);
	ctype = stringFortToC(type, typeLen);
	

	if (*boolDoubles) {
		tss = zstructTsNewIrregDoubles(pathname, doubleValues, *numberValues, timeArray,		 
									   MINUTE_GRANULARITY, cnull, cunits, ctype);
	}
	else {
		tss = zstructTsNewIrregFloats(pathname, singleValues, *numberValues, timeArray,		 
									   MINUTE_GRANULARITY, cnull, cunits, ctype);
	}
	tss->julianBaseDate = *julianBaseDate;
	tss->precision = -1;

	if ((*numberCoordinates == 3) || (*numbCoordDescription == 6)) {
		tss->locationStruct->xOrdinate = coordinates[0];
		tss->locationStruct->yOrdinate = coordinates[1];
		tss->locationStruct->zOrdinate = coordinates[2];
		tss->locationStruct->coordinateSystem = coordinateDescription[0];
		tss->locationStruct->coordinateID = coordinateDescription[1];
		tss->locationStruct->horizontalUnits = coordinateDescription[2];
		tss->locationStruct->horizontalDatum = coordinateDescription[3];
		tss->locationStruct->verticalUnits = coordinateDescription[4];
		tss->locationStruct->verticalDatum = coordinateDescription[5];
	} 


/*	if (timeZoneName && (timeZoneNameLen > 0)) {
		tss->timeZoneName = stringFortToC(timeZoneName, timeZoneNameLen);
		tss->allocated[zSTRUCT_timeZoneName] = 1;
	}
	*/

	if (*boolQuality) {
		tss->quality = quality;
		tss->qualityElementSize = 1;
	}

	if (*userHeaderNumber) {
		tss->userHeader = userHeader;
		tss->userHeaderNumber = *userHeaderNumber;
		tss->userHeaderSize = *userHeaderNumber;
	}

	*status = ztsStore(ifltab, tss, *storageFlag);

	free(pathname);
	free(cunits);
	free(ctype);
	zstructFree(tss);

}