#include <string.h>

#include "heclib.h"
#include "verticalDatum.h"


//  Full method to store regular interval time series data,
//  passing arguments instead of a struct
//  Used for Fortran compatibility 

int ztsStoreRegArgs(long long *ifltab, const char *pathname,  
			const char *startDate, const char *startTime, 
			int numberValues, int *values, int valueSize,
			int *quality, int qualityElementSize,
			int *inotes, int inoteElementSize,	
			char *cnotes, int cnotesLengthTotal,
			int *userHeader, int userHeaderNumber,
			char *units, char *type, 
			int precisionValues, char *timeZoneName, 
			double *coordinates, int *coordinateDescription, int boolCoordinatesUsed,
			int storageFlag)
{
	int boolQuality;
	int boolDouble;
	int status;
	int compression;
	float baseValue;
	int setBase;
	int setDeltaHigh;
	int deltaPrec;
	int zero=0;
	char timezone[40];

	double *coords;
	int *coordDesc;
	double dummy[3];
	int   idummy[6];
	int numberCoords;
	int numberDescription;
	int lenuserHeader;
	char *csupp;
	char cdummy[2];

	int *qualityPassed;
	char unitsPassed[24];
	char typePassed[24];

	zStructTimeSeries *tss;


	idummy[0] = 0;

	if (zgetVersion(ifltab) == 7) { 
		//  Convert to a time series struct and then ztsStoreReg7

		if (valueSize == 1) {
			tss = zstructTsNewRegFloats(pathname, (float *)values, numberValues,
										 startDate, startTime, units, type);
		}
		else if (valueSize == 2) {
			tss = zstructTsNewRegDoubles(pathname, (double *)values, numberValues,
										 startDate, startTime, units, type);
		}
		else {
			return -1;
		}
		
		if ((quality) && (qualityElementSize > 0)) {
			tss->quality = quality;
			tss->qualityElementSize = qualityElementSize;
		}

		if ((inotes) && (inoteElementSize > 0)) {
			tss->inotes = inotes;
			tss->inoteElementSize = inoteElementSize;
		}

		if ((cnotes) && (cnotesLengthTotal > 0)) {
			tss->cnotes = cnotes;
			tss->cnotesLengthTotal = cnotesLengthTotal;
		}

		if ((userHeader) && (userHeaderNumber > 0)) {
			tss->userHeader = userHeader;
			tss->userHeaderNumber = userHeaderNumber;
		}

		if (units) tss->units = units;
		if (type) tss->type = type;	
		if (timeZoneName) tss->timeZoneName = timeZoneName;	
		tss->precision = precisionValues;

		if (boolCoordinatesUsed) {
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

		status = ztsStore(ifltab, tss, storageFlag);

		zstructFree(tss);

	}
	else if (zgetVersion(ifltab) == 6) { 

		if (valueSize == 1) {
			boolDouble = 0;
		}
		else {
			boolDouble = 1;
		}

		if (qualityElementSize == 1) {
			boolQuality = 1;
		}
		else {
			boolQuality = 0;
		}

		if (boolCoordinatesUsed) {
			coords = coordinates;
			coordDesc = coordinateDescription;
			numberCoords = 3;
			numberDescription = 6;
		}
		else {
			coords = dummy;
			coordDesc = idummy;
			numberCoords = 0;
			numberDescription = 0;
		}
		
		if ((userHeader) && (userHeaderNumber > 0)) {
			csupp = userHeaderToString(userHeader, userHeaderNumber);
			lenuserHeader = userHeaderNumber * 4;
		}
		else {
			lenuserHeader = 0;
			cdummy[0] = '\0';
			csupp = cdummy;
		}

		if (timeZoneName) {
			stringCToFort(timezone, sizeof(timezone), timeZoneName);
		}
		else {
			stringFill(timezone, '\0', sizeof(timezone));
		}


		if (quality) {
			qualityPassed = quality;
		}
		else {
			qualityPassed = idummy;
		}

		if (units) {
			stringCToFort(unitsPassed, sizeof(unitsPassed), units);
		}
		else {
			stringFill(unitsPassed, ' ', sizeof(unitsPassed));
		}

		if (type) {
			stringCToFort(typePassed, sizeof(typePassed), type);
		}
		else {
			stringFill(typePassed, ' ', sizeof(typePassed));
		}

		compression = 0;
		baseValue = 0.0;
		setBase = 0;
		setDeltaHigh = 0;
		deltaPrec = 0;
		zset6_("PREC", " ", &precisionValues, 4, 0);

		zsrtsc6_ (ifltab, pathname, startDate, startTime, &numberValues, 
			&boolDouble, (float *)values, (double *)values,
			qualityPassed, &boolQuality,
			unitsPassed, typePassed,
			coords, &numberCoords, coordDesc, &numberDescription,
			csupp, &zero, timezone, 			
			&storageFlag, &compression, &baseValue, &setBase, &setDeltaHigh,
			&deltaPrec, &status,
			strlen (pathname), strlen (startDate), strlen (startTime),
			sizeof(unitsPassed), sizeof(typePassed), (size_t) lenuserHeader,
			strlen(timezone));

		if (userHeaderNumber > 0) {
			free(csupp);
		}
/*
SUBROUTINE zsrtsc6 ( IFLTAB, CPATH, CDATE, CTIME, NVALS,
* LDOUBLE, VALUES, DVALUES, JQUAL, LQUAL, CUNITS, CTYPE,
* COORDS, NCOORDS, ICDESC, NCDESC, CSUPP, ITZONE,
* CTZONE, IPLAN, JCOMP, BASEV, LBASEV, LDHIGH, NPREC, ISTAT)
*/

	}
	else {
		//  Fix me!!!   really bad error
		return STATUS_NOT_OKAY;
	}
	return status;	
}


