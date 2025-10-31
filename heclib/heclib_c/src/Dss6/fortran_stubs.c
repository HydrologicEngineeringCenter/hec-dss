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


