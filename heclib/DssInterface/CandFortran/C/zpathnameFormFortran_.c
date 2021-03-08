#include <stdlib.h>

#include "heclib7.h"
#include "hecdssInternal.h"
//#include "hecdss_6.h"

void zpathnameform_(const char *aPart, const char *bPart, const char *cPart, const char *dPart,
	const char *ePart, const char *fPart, char *pathname,
	size_t aPartLen, size_t bPartLen, size_t cPartLen, size_t dPartLen,
	size_t ePartLen, size_t fPartLen, size_t sizeofPathname)
{
	char *caPart;
	char *cbPart;
	char *ccPart;
	char *cdPart;
	char *cePart;
	char *cfPart;
	char path[MAX_PATHNAME_LENGTH];

	caPart = stringFortToC(aPart, aPartLen);
	cbPart = stringFortToC(bPart, bPartLen);
	ccPart = stringFortToC(cPart, cPartLen);
	cdPart = stringFortToC(dPart, dPartLen);
	cePart = stringFortToC(ePart, ePartLen);
	cfPart = stringFortToC(fPart, fPartLen);

	zpathnameForm(caPart, cbPart, ccPart, cdPart,
	cePart, cfPart, path, sizeof(path));

	stringCToFort(pathname, sizeofPathname,  path);

	free(caPart);
	free(cbPart);
	free(ccPart);
	free(cdPart);
	free(cePart);
	free(cfPart);
}

