#include "heclib.h"


/// <summary>
/// test sending DSS logging to a file
/// </summary>
/// <returns></returns>
int test_logging() {

	// log without ifltab reference.
	long long ifltab[250] = {0};
	const char* logFilename = "test_log{101}.txt";
	deleteFile(logFilename);

	int status = zopenLog(logFilename); // sets zdssVals.messageHandle 

	if (status != 0) {
		return status;
	}
	char* msg = "Write to the Log... (dss file not opened)";
	zmessageLen(ifltab, msg, strlen(msg));


	const char* dssFilename = "log_test{101}.dss";
	deleteFile(dssFilename);
	hec_dss_zopen(ifltab, dssFilename);

	zcloseLog(); // resets log handle to zero
	zclose(ifltab);
	// logging will now be to console



	return 0;
}