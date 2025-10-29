#include <string.h>

#include "heclib.h"
#include "hecdssInternal.h"

int hec_dss_zopen(long long *ifltab, const char *dssFilename)
{

	if (!dssFilename || strlen(dssFilename) <=0 ) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zopen_ID, zdssErrorCodes.NULL_FILENAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
	}

	int version = zgetFileVersion(dssFilename);
	if (zmessageLevel(ifltab, MESS_METHOD_OPEN_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_zopen_ID, "Entering zopenExtended for file: ", dssFilename);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zopen_ID, "Dss file version ", version);
	 }


	return zopenInternal(ifltab, dssFilename, 0, 0, 0, 0, 0);
}


