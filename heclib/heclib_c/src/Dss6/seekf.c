#ifdef _MSC_VER
#include <io.h>
#include <stdio.h>
#else

#if defined(__linux__)
#define _LARGEFILE64_SOURCE 
#endif 

#include <unistd.h>
#include <stdint.h>
#endif


void
seekf64_ (int *ihandl, int *iorigin, long long*iofset, long long*ipos, int *istat)
{
#ifdef _MSC_VER
    *ipos = _lseeki64(*ihandl, *iofset, *iorigin);
#else
	*ipos = lseek64(*ihandl, *iofset, *iorigin);
#endif
    *istat = ((*ipos == -1) ? -1 : 0);
}

