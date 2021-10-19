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
*  Function:	zopenDisk
*
*  Use:			Semi-Public
*
*  Description:	Low level open file.
*
*  Declaration: int zopenDisk (const char *cname, int *handle, int iaccess, int exclusiveAccess)
*
*  Parameters:	const char *fileName
*					The name of the file to open.
*
*				int *handle (output)
*					The handle or file pointer to the opened file.  If an error, this is undefined.
*
*				int iaccess
*					The combined access code to open the file with.  Valid binaries are:
*						O_RDONLY (0)  open for reading only
*						O_WRONLY (1)  open for writing only
*						O_RDWR  (2)  open for reading and writing
*						O_CREAT (8)  If does not exist, open, then access for reading and writing
*					Generally, 10 is used, which is O_RDWR + O_CREAT
*
*				int exclusiveAccess
*					A boolean flag to access the file exclusively.  Set to zero for normal
*					(non-exclusive), one for exclusive access.  Not guaranteed.
*
*
*	Returns:	Status:
*					STATUS_OKAY for successful operation.
*					errorCode for error
*
*	Note:		Almost always opened with iaccess = 10, and exclusiveAccess = 0.
*				Machine specific function.
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zopenDisk (const char *cname, int *handle, int iaccess, int exclusiveAccess)
{

      int acc = 0;
      if ((iaccess & 0) != 0) {
            acc |= O_RDONLY;
      }
      if ((iaccess & 1) != 0) {
            acc |= O_WRONLY;
      }
      if ((iaccess & 2) != 0) {
            acc |= O_RDWR;
      }
      if ((iaccess & 4) != 0) {

      }
      if ((iaccess & 8) != 0) {
            acc |= O_CREAT;
	  }

#ifdef _MSC_VER

	  acc |= _O_BINARY;
	  acc |= _O_RANDOM;

	  if (exclusiveAccess != 0) {
		  return _sopen_s(handle, cname, acc, _SH_DENYRW, _S_IWRITE);
	  }
	  else {
		  return _sopen_s(handle, cname, acc, _SH_DENYNO, (_S_IREAD | _S_IWRITE));
	  }
#else
	  *handle = open(cname, acc, 0666);
	  if (*handle > 0) {
		  return 0;
	  }
	  else {
		  return -1;
	  }
#endif

}

