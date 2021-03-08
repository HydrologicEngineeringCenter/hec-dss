
#ifdef _MSC_VER
#include <io.h>
#else
#include <unistd.h>
#include <stdint.h>
#endif

#include "heclib6.h"

void writf_(int *ihandle, void *buff, int *nbytes, int *istat, int *ntrans)
{
#ifdef _MSC_VER
	*ntrans = _write(*ihandle, buff, (size_t)*nbytes);
#else
	*ntrans = write(*ihandle, buff, *nbytes);
#endif
	*istat = *ntrans >= 0 ? 0 : -1;
}

