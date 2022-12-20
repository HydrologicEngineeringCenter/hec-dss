#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdlib.h>

#include "heclib6.h"
#include "hecdss7.h"
#include "heclib.h"
#include "zdssMessages.h"
#include "fortran_string_len_size.h"

/**
*  Function:	zsqueeze
*
*  Use:			Public
*
*  Description:	Interface for DSS version 6 and version 7.
*					A squeeze rebuilds a file, which removes inactive space,
*					rebuilds internal tables and adjust table sizes to optimize data access.
*					Squeezing uses a brute force approach, which will recover any data sets that may
*					have broken links (rare), usually from a crash or disk damage.
*					This is similar to de-fragmenting your file system.��
*					Once a squeeze has been accomplished, deleted data cannot be recovered.
*
*  Declaration: int zsqueeze(const char *dssFilename);
*
*  Parameters:	const char *dssFilename
*					The name of the file to squeeze.  You must be able to have exclusive access -
*					no one else can have any access to the file during a squeeze.
*
*
*
*	Returns:	int status
*					STATUS_OKAY for successful operation.
*					errorCode (< 0) should an error occur.
*
*	See Also:	int zsqueeze7(long long *ifltab, int boolOnlyIfNeeded, int boolInPlace);
*				int zsqueezeNeeded(long long *ifltab);
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zsqueeze(const char *dssFilename)
{


	char tempDssFilename[_MAX_PATH] = {0};
	long long ifltab[600] = {0};
	int version;
	int status;


#ifdef _MSC_VER
	if (_fullpath(tempDssFilename, dssFilename, _MAX_PATH) == NULL) {
		printf("Illegal file name or cannot determine file name, name: %s\n", dssFilename);
		ifltab[0] = 0;
		return zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID,
			zdssErrorCodes.INVALID_FILE_NAME, 0, 0,
			zdssErrorSeverity.WARNING_NO_WRITE_ACCESS, "", tempDssFilename);
	}
#else
	char *fullName;
	fullName = realpath(dssFilename, (char*)0);
	if (fullName == NULL) {
		// printf("Illegal file name or cannot determine file name, name: %s\n", dssFilename);
		// ifltab[0] = 0;
		// return zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID,
		//	zdssErrorCodes.INVALID_FILE_NAME, 0, 0,
		//	zdssErrorSeverity.WARNING_NO_WRITE_ACCESS, "", dssFilename);
		//  If we cannot get the full file name (with path), just use what we were given and hope for the best.
		stringCopy(tempDssFilename, sizeof(tempDssFilename), dssFilename, strlen(dssFilename));
	}
	else {
		stringCopy(tempDssFilename, sizeof(tempDssFilename), fullName, strlen(fullName));
		free(fullName);
	}
#endif

	if (!dssFilename) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zsqueeze_ID, zdssErrorCodes.NULL_FILENAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "");
	}


   version = zgetFileVersion(tempDssFilename);
   if (version != 7) {
	   //  Let version 6 function deal with
	   zsqueeze6_(tempDssFilename, &status, strlen(tempDssFilename));

   }
   else {
	   status = zopenInternal(ifltab, tempDssFilename, 0, 0, 0, 0, 0);
	   if (zisError(status)) {
		   printf("File is in use and cannot be squeezed, name: %s\n", tempDssFilename);
		   return status;
		}
	   status = zsqueeze7(ifltab, 0, 1);
	   zclose(ifltab);
   }
   return status;

}

void zsqueeze_(const char *dssFilename, int *istatus, slen_t fileNameLength)
{
	char *filename;

	filename = stringFortToC(dssFilename, fileNameLength);
	*istatus = zsqueeze(filename);
	free(filename);

}

void zsqueeze7_(const char *dssFilename, int *istatus, slen_t fileNameLength)
{
	char *filename;

	filename = stringFortToC(dssFilename, fileNameLength);
	*istatus = zsqueeze(filename);
	free(filename);

}

