
#ifdef _MSC_VER
#include <io.h>
#else
#include <unistd.h>
#include <stdint.h>
#endif

#include "hecdssInternal.h"

int closeFile (int handle)
{
#ifdef _MSC_VER
    return _close(handle);
#else
	return close(handle);
#endif
}

