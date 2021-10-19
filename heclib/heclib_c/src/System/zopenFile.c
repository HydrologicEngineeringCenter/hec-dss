#ifdef _MSC_VER
#include <io.h>
#include <errno.h>
#include <share.h>
#else
#include <unistd.h>
#include <stdint.h>
#endif

#include <fcntl.h>
#include <sys/stat.h>


#include "hecdssInternal.h"

/**
*  Function:	zopenFile
*
*  Use:			Semi-Public
*
*  Description:	Low level open file.
*
*  Declaration: int zopenFile (const char *cname, int iaccess)
*
*  Parameters:	const char *fileName
*					The name of the file to open.

*				int iaccess
*					The combined access code to open the file with.  Valid binaries are:
*						O_RDONLY (0)  open for reading only
*						O_WRONLY (1)  open for writing only
*						O_RDWR  (2)  open for reading and writing
*						O_CREAT (8)  If does not exist, open, then access for reading and writing
*
*
*	Returns:	handle:
*					> 0 for successful operation.
*					negative for error

*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zopenFile(const char *cname, int iaccess)
{
	int handle;
	int status;
#ifdef _MSC_VER
	status = _sopen_s(&handle, cname, iaccess, _SH_DENYNO, (_S_IREAD | _S_IWRITE));
#else
	handle = open(cname, iaccess, 0666);
#endif
	return handle;
}

