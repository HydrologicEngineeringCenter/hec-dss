#include <string.h>

#include "heclib7.h"
#include "hecdssInternal.h"
#include "fortran_string_len_size.h"



//  Fortran Interface
void zpathnamegetpart_ (const char *pathname, int *partPosition, char *part, slen_t pathnameLength, slen_t sizeofPart)
{
	char cPart[MAX_PART_SIZE];

	zpathnameGetPart (pathname, *partPosition, cPart, sizeof(cPart));
	stringCToFort(part, sizeofPart,  cPart);
}

