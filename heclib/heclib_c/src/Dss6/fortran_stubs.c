#include <stdlib.h>
#include <stdio.h>

void dss_stub_called(const char* caller) {
	printf("\nWARNING: DSS Stub called from %s.", caller);
}

#define DSS_STUB_CALLED() dss_stub_called(__func__)


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
