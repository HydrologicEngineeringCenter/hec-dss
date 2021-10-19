#ifdef _MSC_VER
#include <io.h>
#include <errno.h>
#else
#include <unistd.h>
#include <stdint.h>
#endif

void
flushf_ (int *ihandle, int *istat)
{

#ifdef _MSC_VER
	*istat = _commit(*ihandle);
#else
	*istat = fsync(*ihandle);
#endif
}

