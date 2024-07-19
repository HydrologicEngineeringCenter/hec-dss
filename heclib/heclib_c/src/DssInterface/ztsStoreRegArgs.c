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

	} else {
		//  Fix me!!!   really bad error
		return STATUS_NOT_OKAY;
	}
	return status;	
}
