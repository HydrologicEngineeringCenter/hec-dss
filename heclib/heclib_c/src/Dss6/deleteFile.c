#include <stdio.h>
#if defined(__linux__) || defined(__APPLE__)
#include <unistd.h>
#endif
#include "heclib.h"

int deleteFile(const char *filename)
{
#ifdef _MSC_VER
	int istat;
	istat = _unlink(filename);
	_set_errno(0);
	return istat;
#else
	return unlink(filename);
#endif
}

void deletefile_(const char *filename, int *status, size_t lenFilename)
{
	char *name;

	name = stringFortToC(filename, lenFilename);
	*status = deleteFile((const char *)name);
	free(name);
}

