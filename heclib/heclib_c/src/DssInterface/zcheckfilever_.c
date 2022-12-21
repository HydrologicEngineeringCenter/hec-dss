#include "heclib.h"
#include "hecdssInternal.h"



//  Fortran compatible interface

//  FIX ME
void zgetfileversion_(const char *dssFilename, int *iver, size_t lenDssFilename)
{
	char *cname;

	cname = stringFortToC(dssFilename, lenDssFilename);
	if (!cname) {
		return;
	}

	*iver = zgetFileVersion(cname);
	free(cname);
}

void zcheckfilever_(const char *dssFilename, int *iver, size_t lenDssFilename)
{
	char *cname;

	cname = stringFortToC(dssFilename, lenDssFilename);
	*iver = zgetFileVersion(cname);
	free(cname);
}

