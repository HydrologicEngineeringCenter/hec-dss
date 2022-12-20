#include <string.h>

#include "hecdssInternal.h"
#include "heclib.h"
#include "fortran_string_len_size.h"

/*
	zopen interfaces for Fortran

	For all functions, zopen will open with the appropriate version,
	if the file already exists (i.e., you can call zopen6 on a 7 file)

	If the file does NOT exist, then
		zopen will create a version 7 file
		zopen6 will create a version 6 file
		zopen7 will create a version 7 file (same as zopen)

*/


void zopen_(long long *ifltab, const char *dssFilename, int *status, slen_t lenDssFilename)
{
	int version;
	char *cname;
	char ctemp[1];

	cname = stringFortToC(dssFilename, lenDssFilename);
	version = zgetFileVersion(cname);

	//  Check for a specific version set for the next new file
	if (version == 0) {
		//  File does not exist
		zquery_("DSSV", ctemp, &version, (size_t)4, sizeof(ctemp));
		//  If set, reset to zero
		if (version > 0) {
			zset("DSSV", "", 0);
		}
	}

	if (version == 6) {
		zopen6int_(ifltab, cname, status, strlen(cname));
	}
	else {
		*status = zopenInternal(ifltab, cname, 0, 0, 0, 0, 0);
	}
	free(cname);
}

void zopen7_(long long *ifltab, const char *dssFilename, int *status, slen_t lenDssFilename)
{
	int version;
	char *cname;

	cname = stringFortToC(dssFilename, lenDssFilename);
	version = zgetFileVersion(cname);

	if (version == 6) {
		zopen6int_(ifltab, cname, status, strlen(cname));
	}
	else {
		*status = zopenInternal(ifltab, cname, 0, 0, 0, 0, 0);
	}
	free(cname);
}

void zopen6_(long long *ifltab, const char *dssFilename, int *status, slen_t lenDssFilename)
{
	int version;
	char *cname;

	cname = stringFortToC(dssFilename, lenDssFilename);
	version = zgetFileVersion(cname);

	if (version == 7) {
		*status = zopenInternal(ifltab, cname, 0, 0, 0, 0, 0);
	}
	else {
		zopen6int_(ifltab, cname, status, strlen(cname));
	}
	free(cname);
}

