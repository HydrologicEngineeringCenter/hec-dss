#ifdef _MSC_VER
#include <io.h>
#include <errno.h>
#else
#include <unistd.h>
#include <errno.h>
#include <stdint.h>
#endif

void
readf_ (int *ihandl, void *buff, int *nbytes, int *istat, int *ntrans)
{
#ifdef _MSC_VER
    *ntrans = _read (*ihandl, buff, (size_t)*nbytes);
    *istat  = ((*ntrans >= 0) ? 0 : -1);
	if (*istat)
		*istat = errno;
#else
	*ntrans = read(*ihandl, buff, *nbytes);
	*istat = *ntrans >= 0 ? 0 : errno;
#endif
}

