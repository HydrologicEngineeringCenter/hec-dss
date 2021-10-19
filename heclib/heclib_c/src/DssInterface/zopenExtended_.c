#include <string.h>

#include "hecdssInternal.h"
#include "heclib.h"

int zopenextended_(long long *ifltab, const char *dssFilename, int *fileVersion,
			 int *access, int *maxExpectedPathnames, int *hashSize, int *binSize, size_t lenDssFilename)
{
	int version;
	int status;
	char *cname;

	cname = stringFortToC(dssFilename, lenDssFilename);
	version = zgetFileVersion(cname);

	if ((version == 6) || ((version == 0) && (*fileVersion == 6))) {
		zopen6_(ifltab, cname, &status, strlen(cname));
	}
	else {
		status = zopenInternal(ifltab, cname, *access, *maxExpectedPathnames, *hashSize, *binSize, 0);
	}
	free(cname);
	return status;
}

