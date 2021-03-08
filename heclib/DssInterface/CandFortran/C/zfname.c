#include <stdlib.h>

#include "heclib.h"
#include "hecdssInternal.h"



void zfname(const char *dssFilenameIn, char *dssFilenameOut, int *nname, int *exists,
			size_t lenDssFilenameIn, size_t sizeDssFilenameOut)
{
	int idummy;

	*exists = zfileName(dssFilenameOut, sizeDssFilenameOut, dssFilenameIn, &idummy);

}

