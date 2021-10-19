#ifdef _MSC_VER
#include <io.h>
#else
#include <unistd.h>
#include <stdint.h>
#endif

#include "hecdssInternal.h"

int flushFile(int ihandle)
{

#ifdef _MSC_VER
	return _commit(ihandle);
#else
	return fsync(ihandle);
#endif
}

