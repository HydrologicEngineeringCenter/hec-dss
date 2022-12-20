#include <stdlib.h>

#include "heclib7.h"
#include "hecdssInternal.h"
#include "fortran_string_len_size.h"
//#include "hecdss_6.h"

void zpathnameform_(const char *aPart, const char *bPart, const char *cPart, const char *dPart,
	const char *ePart, const char *fPart, char *pathname,
	slen_t aPartLen, slen_t bPartLen, slen_t cPartLen, slen_t dPartLen,
	slen_t ePartLen, slen_t fPartLen, slen_t sizeofPathname)
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

