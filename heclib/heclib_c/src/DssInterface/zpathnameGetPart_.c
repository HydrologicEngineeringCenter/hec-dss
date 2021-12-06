#include <string.h>

#include "heclib7.h"
#include "hecdssInternal.h"



//  Fortran Interface
void zpathnamegetpart_ (const char *pathname, int *partPosition, char *part, size_t pathnameLength, size_t sizeofPart)
{
	char cPart[MAX_F_PART_SIZE];

	zpathnameGetPart (pathname, *partPosition, cPart, sizeof(cPart));
	stringCToFort(part, sizeofPart,  cPart);
}

