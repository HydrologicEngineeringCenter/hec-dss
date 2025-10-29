#include <stdlib.h>
#include <stdio.h>

void dss_stub_called(const char* caller) {
	printf("\nWARNING: DSS Stub called from %s.", caller);
}

#define DSS_STUB_CALLED() dss_stub_called(__func__)



void zset6_(const char* parameter, const char* charVal, int* integerValue, size_t lenparameter, size_t lencharVal) {
	DSS_STUB_CALLED();
}

void zreadx6_(long long* ifltab, const char* path, int* internalHeader, int* internalHeaderArraySize, int* internalHeaderNumber, int* header2, int* header2ArraySize, int* header2Number, int* userHeader, int* userHeaderArraySize, int* userHeaderNumber, int* values, int* valuesSize, int* valuesNumber, int* readPlan, int* recordFound, size_t pathLen) {
	DSS_STUB_CALLED();
}

void zgetrecsize6_(long long* ifltab, const char* pathname, int* nihead, int* nchead, int* nuhead, int* ndata, int* istatus, size_t pathLen) {
	DSS_STUB_CALLED();
}

void zcheck6_(long long* ifltab, const char* pathname, int* numberPathname, int* numberHeader, int* numberData, int* lfound, size_t lenPathname) {
	DSS_STUB_CALLED();
}

void ztsends6_(int* ifltab, const char* cpath, int* searchOption, int* startJulian, int* startMinutes, int* endJulian, int* endMinutes, int* exists, size_t len_cpath) {
	DSS_STUB_CALLED();
}

void zdelet6_(long long* ifltab, const char* pathname, int* numberPathname, int* boolfound, size_t lenPathname) {
	DSS_STUB_CALLED();
}

void zwritex6_(long long* ifltab, const char* path, int* npath, int* internalHeader, int* internalHeaderNumber, int* header2, int* header2Number, int* userHeader, int* userHeaderNumber, int* values, int* valuesNumber, int* dataType, int* plan, int* status, int* recordFound, size_t pathLen) {
	DSS_STUB_CALLED();
}

void zsqueeze6_(const char* dssFilename, int* status, size_t lenDssFilename) {
	DSS_STUB_CALLED();
}

void zundel6_(long long* ifltab, const char* pathname, int* numberPathname, int* status, size_t lenPathname) {
	DSS_STUB_CALLED();
}

void zrenam6_(long long* ifltab, const char* oldPathname, int* len_oldPathname, const char* newPathname, int* len_newPathname, int* status, size_t length_oldPathname, size_t length_newPathname) {
	DSS_STUB_CALLED();
}

void zread6_(long long* ifltab, const char* path, int* npath, int* userHeader, int* userHeaderNumber, int* values, int* valuesNumber, int* readFlag, int* recordFound, size_t pathLen) {
	DSS_STUB_CALLED();
}

void zwrite6_(long long* ifltab, const char* path, int* npath, int* userHeader, int* userHeaderNumber, int* values, int* valuesNumber, int* flag, int* recordFound, size_t pathLen) {
	DSS_STUB_CALLED();
}

void zquery6_(const char* request, char* returnVal, int* returnNumb, size_t len_request, size_t sizeof_returnVal) {
	DSS_STUB_CALLED();
}

void zdtype_(long long* ifltab, const char* arg1, int* arg2, int* arg3, char* arg4, int* arg5, size_t arg6, size_t arg7) {
	DSS_STUB_CALLED();
}

void zrrtsi6_(long long* ifltab, const char* pathname, const char* startDate, const char* startTime, int* maxVals, int* numberVals, int* getDoubles, int* doublesRead, float* floatValues, double* doubleValues, int* flags, int* readFlags, int* flagsRead, char* units, char* type, int* userHeader, int* maxUserHead, int* numberUserHead, int* offset, int* compression, double* coordinates, int* coordinateDescription, int* coordinatesUsed, int* status, size_t lenPathname, size_t lenStartDate, size_t lenStartTime, size_t lenUnits, size_t lenType) {
	DSS_STUB_CALLED();
}

void zgettz_(char* ctzone, int* itzone, size_t size_ctzone) {
	DSS_STUB_CALLED();
}

void zritsi6_(long long* ifltab, const char* path, int* startJulian, int* startTimeMinutes, int* endJulian, int* endTimeMinutes, int* boolGetDoubles, int* boolDoublesRead, int* timeArray, float* singles, double* doubles, int* maxNumberValues, int* numberRead, int* julianBaseDate, int* quality, int* boolGetQuality, int* boolQualityRead, char* units, char* type, int* userHeader, int* userHeaderArraySize, int* userHeaderNumber, double coordinates[], int coordinateDescription[], int* boolCoordinatesRead, int* readFlag, int* status, size_t pathLen, size_t unitsLen, size_t typeLen) {
	DSS_STUB_CALLED();
}

void zrecordinfo6_(long long* ifltab, const char* pathname, int* idataType, int* iversion, int* numberVals, int* logicalNumberVals, int* numberVals1, int* numberInternalHead, int* numberUserHead, int* isizeAllocated, int* lastWriteSecs, int* iprecisionTS, int* ioffset, int* ivalSize, int* iqualSize, int* numberCurvesPD, int* numberOrdinatesPD, int* ipdValueSize, int* iaxisFlagPD, int* lboolLabelsPD, int* iprecisionPD, int* istatus, size_t lenPathname) {
	DSS_STUB_CALLED();
}

int fortranwritelc_(int* fortranUnit, const char* message, int* boolEndOfRecord, size_t lengthMessage) {
	DSS_STUB_CALLED();
	return 0;
}

void flush_(int* fortranUnit) {
	DSS_STUB_CALLED();
}

void zrdprm6_(long long* ifltab, int* istatus) {
	DSS_STUB_CALLED();
}

void zspdi6_(long long* ifltab, const char* pathname, int* numberOrdinates, int* numberCurves, int* iHorizontal, const char* unitsIndependent, const char* typeIndependent, const char* unitsDependent, const char* typeDependent, int* svalues, int* dvalues, int* boolDoubles, const char* labels, int* boolStoreLabels, int* userHeader, int* userHeaderNumber, int* iplan, int* istat, size_t pathnameLen, size_t unitsIndependentLen, size_t typeIndependentLen, size_t unitsDependentLen, size_t typeDependentLen, size_t lablesLength) {
	DSS_STUB_CALLED();
}

void chrlnb_(char* cstring, int* len, size_t strlen) {
	DSS_STUB_CALLED();
}

void zrpdi6_(long long* ifltab, const char* pathname, int* numberOrdinates, int* numberCurves, int* iHorizontal, char* unitsIndependent, char* typeIndependent, char* unitsDependent, char* typeDependent, float* svalues, double* dvalues, int* boolDoubles, int* maxValues, int* numberValues, char* labels, int* maxLables, int* boolLabels, int* userHeader, int* maxUserHead, int* userHeaderNumber, int* istat, size_t pathnameLen, size_t unitsIndependentLen, size_t typeIndependentLen, size_t unitsDependentLen, size_t typeDependentLen, size_t lablesLength) {
	DSS_STUB_CALLED();
}

void zgetinfo6_(long long* ifltab, char* pathname, int* status, int* iarray, size_t len_pathname) {
	DSS_STUB_CALLED();
}

void ztsinfo_(long long* ifltab, const char* cpath, int* juls, int* istime, int* jule, int* ietime, char* cunits, char* ctype, int* lqual, int* ldouble, int* lfound, size_t cpath_len, size_t cunits_len, size_t ctype_len) {
	DSS_STUB_CALLED();
}

void zsrtsc6_(long long* ifltab, const char* pathname, const char* startDate, const char* startTime, int* numberVals, int* lDouble, float* floatVaues, double* doubleValues, int* flags, int* storeFlags, const char* units, const char* type, double* coordinates, int* numberCoordinates, int* coordinateDescription, int* numberCoordDescription, const char* supplementaryInfo, int* timezoneOffset, const char* timezoneName, int* plan, int* compression, float* baseCompress, int* setBaseComp, int* setDeltaHigh, int* prec, int* status, size_t lenPathname, size_t lenStartDate, size_t lenStartTime, size_t lenUnits, size_t lenType, size_t lenSupplementaryInfo, size_t lenTimezoneName) {
	DSS_STUB_CALLED();
}

void zsitsc6_(long long* ifltab, const char* pathname, const int* timeArray, const float* floatValues, const double* doubleValues, int* storeDoubles, int* numberValues, int* baseDate, int* flags, int* storeFlags, const char* units, const char* type, double* coordinates, int* numberCoordinates, int* coordinateDescription, int* numberCoordDescription, const char* supplementaryInfo, int* timezoneOffset, const char* timezoneName, int* inflag, int* status, size_t lenPathname, size_t lenUnits, size_t lenType, size_t lenSupplementaryInfo, size_t lenTimezoneName) {
	DSS_STUB_CALLED();
}

void zcolist6_(long long* ifltab, int* filePos, char* pathname, int* nPathname, int* status, size_t sizeof_pathname) {
	DSS_STUB_CALLED();
}

void zmaxpart6_(long long* ifltab, int* maxParts) {
	DSS_STUB_CALLED();
}

void zplist6_(long long* ifltab, const char* instr, int* filePos, char* pathname, int* nPathname, int* status, size_t len_instr, size_t sizeof_pathname) {
	DSS_STUB_CALLED();
}

void ztsrange_(long long* ifltab, const char* cpath, int* searchOption, char* firstPath, char* lastPath, int* numberFound, size_t len_cpath, size_t len_firstPath, size_t len_lastPath) {
	DSS_STUB_CALLED();
}
