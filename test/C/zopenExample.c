#include <stdio.h>
#include "heclib.h"

// If MacOS, use hec_zopen instead of stdio::zopen
#ifdef __APPLE__
#define zopen hec_zopen
#endif

int main(int argc, char* argv[])
{
	long long ifltabIn[250];
	long long ifltabOut[250];
	int status;
	int exists;
	char fileIn[] = "zopenExampleReceived.dss";
	char fileOut[] = "zopenExampleProcessed.dss";

	 
	status = zopen(ifltabIn, fileIn);
	if (status != 0) {
		if (zmessageAvailable(ifltabIn)) {
			printf("%s\n", zgetMessage(ifltabIn));
			zclearMessage(ifltabIn);
		}
		return -1;
	}
	//  Now open the file we are writing to.  It does not have to exist.
	status = zopen(ifltabOut, fileOut);
	if (status != 0) {
		if (zmessageAvailable(ifltabOut)) {
			printf("%s\n", zgetMessage(ifltabOut));
			zclearMessage(ifltabOut);
		}
		return -1;
	}

	//  Process data  (read and write)

	// All done, close files
	zclose(ifltabIn);
	zclose(ifltabOut);
	printf("OK\n");
	return 0;
}

