#include <string.h>

#include "heclib.h"
#include "hecdssInternal.h"

int hec_dss_zopen(long long *ifltab, const char *dssFilename)
{
	int version;
	int status;
	int zero;
	char creturn[1];


	if (!dssFilename || strlen(dssFilename) <=0 ) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zopen_ID, zdssErrorCodes.NULL_FILENAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
	}

	version = zgetFileVersion(dssFilename);
	if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zopen_ID, "Entering zopenExtended for file: ", dssFilename);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zopen_ID, "Dss file version ", version);
	 }
	//  Check for a specific version set for the next new file
	if (version == 0) {
		//  File does not exist
		zquery_("DSSV",  creturn, &version, (size_t)4, sizeof(creturn));
		//  If set, reset to zero
		if (version) {
			zero = 0;
			zset_("DSSV", creturn, &zero, (size_t)4, sizeof(creturn));
		}
	}

	if (version == 6) {
		// DSS 6 on Linux is not supported..
#ifdef __linux__

	zmessageDebug(ifltab, DSS_FUNCTION_zopen_ID, "---- ERROR -------", dssFilename);
	zmessageDebug(ifltab, DSS_FUNCTION_zopen_ID, "------------------", dssFilename);
    zmessageDebug(ifltab, DSS_FUNCTION_zopen_ID, "DSS version 6 is not supported on Linux ", dssFilename);
	zmessageDebug(ifltab, DSS_FUNCTION_zopen_ID, "------------------", dssFilename);
    return -123;
#else

		zopen6int_(ifltab, dssFilename, &status, strlen(dssFilename));
		return status;
#endif
	}
	else {
		return zopenInternal(ifltab, dssFilename, 0, 0, 0, 0, 0);
	}
}

int zopen7(long long *ifltab, const char *dssFilename)
{
	int version;
	int status;

	version = zgetFileVersion(dssFilename);

	if (version == 6) {
		zopen6int_(ifltab, dssFilename, &status, strlen(dssFilename));
		return status;
	}
	else {
		return zopenInternal(ifltab, dssFilename, 0, 0, 0, 0, 0);
	}
}

int zopen6(long long *ifltab, const char *dssFilename)
{
	int version;
	int status;

	version = zgetFileVersion(dssFilename);

	if (version == 7) {
		return zopenInternal(ifltab, dssFilename, 0, 0, 0, 0, 0);
	}
	else {
		zopen6int_(ifltab, dssFilename, &status, strlen(dssFilename));
		return status;
	}
}

