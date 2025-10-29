#include <stdlib.h>
#include <stdio.h>

void dss_stub_called(const char* caller) {
	printf("\nWARNING: DSS Stub called from %s.", caller);
}

#define DSS_STUB_CALLED() dss_stub_called(__func__)

// TO DO some work on zcatalogInternal.c 
int fortranwritelc_(int* fortranUnit, const char* message, int* boolEndOfRecord, size_t lengthMessage) {
	DSS_STUB_CALLED();
	return 0;
}



void chrlnb_(char* cstring, int* len, size_t strlen) {
	DSS_STUB_CALLED();
}


void ztsinfo_(long long* ifltab, const char* cpath, int* juls, int* istime, int* jule, int* ietime, char* cunits, char* ctype, int* lqual, int* ldouble, int* lfound, size_t cpath_len, size_t cunits_len, size_t ctype_len) {
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
