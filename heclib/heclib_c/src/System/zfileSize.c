#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "hecdssInternal.h"


long long zfileSize(int handle)
{

	long long nbytes;
	int status;
#ifdef _MSC_VER
	struct _stat64 buf;
	status = _fstat64(handle, &buf);
	if (!status) return status;
	nbytes = buf.st_size; 
	return nbytes;
#elif __linux__  || __APPLE__
    struct stat buf;
	status = fstat(handle, &buf);
	if (!status) return status;
	nbytes = buf.st_size; 
	return nbytes;
#else
	struct stat64 buf;
	status = fstat64(handle, &buf);
	if (!status) return status;
	nbytes = buf.st_size;
	return nbytes;
#endif
}

