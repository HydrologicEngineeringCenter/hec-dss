#include <string.h>

#include "heclib.h"
#include "verticalDatum.h"


//  Full method to store irregular interval time series data,
//  passing arguments instead of a struct
//  Used for Fortran compatibility 

int ztsStoreIrregArgs(long long *ifltab, const char *pathname,  
			const char *baseDate, int *itimes, int boolSecondGranularity,
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
	int julianBase;
	int zero=0;
	char *timezone;
	int *qualityPassed;
	char unitsPassed[24];
	char typePassed[24];
	char *csupp;
	char cdummy[5];

	double *coords;
	int *coordDesc;
	double dummy[3];
	int   idummy[6];
	int numberCoords;
	int numberDescription;
	int lenuserHeader;

	zStructTimeSeries *tss;
	char fortran_pathname[MAX_PATHNAME_LENGTH];
	idummy[0] = 0;

	if (zgetVersion(ifltab) == 7) { 
		//  Convert to a time series struct and then ztsStoreReg7

		if (valueSize == 1) {
			tss = zstructTsNewIrregFloats(pathname, (float *)values, numberValues,
				itimes, boolSecondGranularity, baseDate, units, type);
		}
		else if (valueSize == 2) {
			tss = zstructTsNewIrregDoubles(pathname, (double *)values, numberValues,
				itimes, boolSecondGranularity, baseDate, units, type);
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

		if ((baseDate) && (strlen(baseDate) > 4)) {
			julianBase = dateToJulian(baseDate);
			if (julianBase == UNDEFINED_TIME) julianBase = 0;
		}
		else {
			julianBase = 0;
		}

		if (qualityElementSize == 1) {
			boolQuality = 1;
		}
		else {
			boolQuality = 0;
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
			csupp = NULL;
			lenuserHeader = 0;
		}

		if (timeZoneName) {
			timezone = timeZoneName;
		}
		else {
			cdummy[0] = '\0';
			timezone = cdummy;
		}


		zset6_("PREC", " ", &precisionValues, 4, 0);
		stringCToFort(fortran_pathname, sizeof(fortran_pathname),  pathname);

		zsitsc6_ (ifltab, fortran_pathname, itimes,
			(float *)values, (double *)values, &boolDouble, &numberValues, 
			&julianBase, qualityPassed, &boolQuality,
			unitsPassed, typePassed,
			coords, &numberCoords, coordDesc, &numberDescription,
			csupp, &zero, timezone, 
			&storageFlag,  &status,
			strlen(pathname), 
			sizeof(unitsPassed), sizeof(typePassed), (size_t) lenuserHeader,
			strlen(timezone));


	}
	else {
		//  Fix me!!!   really bad error
		return STATUS_NOT_OKAY;
	}
	return status;	
}


