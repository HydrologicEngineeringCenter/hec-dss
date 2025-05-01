#ifdef _MSC_VER
#include <io.h>
#include <errno.h>
#else
#include <unistd.h>
#include <stdint.h>
#endif

#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <errno.h>

#include "heclib7.h"
#include "hecdssInternal.h"


/**
*  Function:	zfileName
*
*  Use:			Public
*
*  Description:	Adds the extension ".dss" if needed.   Returns if the file exists and its permission.
*
*  Declaration:  int zfileName (char *fullDssFilename, size_t sizeofFilename, const char *dssFileName, int *permission)
*  Parameters:	char *fullDssFilename (output)
*					A char array dimensioned to _MAX_PATH, which is returned with the full,
*					absolute DSS file name with drive, path and extension (".dss").
*
*				size_t sizeofFilename
*					The size of fullDssFilename.  Should be _MAX_PATH.
*
*				const char *dssFileName
*					The input name of the DSS file to get the full path to.
*
*				int *permission (output)
*					Returns the file access permission:
*						0 - Normal, read and write or does not exist
*						1 - Read only
*						2 - No access
*
*
*	Returns:	boolFileExists
*					Returns zero if the file does not exist, returns one if the file does exist.
*					Returns STATUS_NOT_OKAY if an error.
*
*	Note:		A  machine / compiler specific function.
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

 int zfileName (char *fullDssFilename, size_t sizeofFilename, const char *dssFileName, int *permission)
 {
#ifdef _MSC_VER
	struct __stat64 buf;
#elif !defined(__sun__)
struct stat buf;
#define stat64 stat
#else
	 struct stat64 buf;
#endif
	int result;
	char name[MAX_FILENAME_LENGTH];
	char *fullName;
	char extension[5];
	int foundExtension;
	int istart;
	size_t len;
	size_t period;

	foundExtension = 0;
	*permission = 0;
	len = strlen(dssFileName);
	//  If a quoted string, remove quotes
	if (dssFileName[len-1] == '\"') {
		len--;
	}
	//  Do we need to add the extension ".dss"?
	if (len > 4) {
		 period = len - 4;
		 stringCopy(extension, sizeof(extension), &dssFileName[period], (size_t)4);
		 lowerCase(extension);
		 if (strncmp(extension, ".dss", 4) == 0) {
			 foundExtension = 1;
		 }
		 else if (strncmp(extension, ".ds-", 4) == 0) {
			 foundExtension = 1;
		 }
	 }
	 //  If a quoted string, remove quotes
	 if (dssFileName[0] == '\"') {
		istart = 1;
		len--;
	 }
	 else {
		istart = 0;
	 }

	 stringCopy(name, MAX_FILENAME_LENGTH, &dssFileName[istart], len);
	 if (!foundExtension) {
		 //  Add extension ".dss", if needed
		 stringCat(name, MAX_FILENAME_LENGTH, ".dss", 4);
	 }


#ifdef _MSC_VER
	 //  Get the absolute pathname, or null if invalid.
	 fullName = _fullpath(fullDssFilename, name, sizeofFilename);
	 if (fullName == NULL) {
		 //  Invalid file name
		 fullDssFilename[0] = '\0';
		 return STATUS_NOT_OKAY;
	 }

	 //  Determine if the file exists
	 buf.st_size = 0;

	result = _stat64(fullName, &buf);
	//  _stat throws an error if the file does not exist - reset it.
	_set_errno(0);
	if (result == 0) {
		//  Get the file permission
		result = _access_s(fullName, 06);
		if (result == 0) {
			//  Normal read/write permission
			*permission = 0;
		}
		else {
			//  Read permission or no access
			result = _access_s(fullName, 04);
			if (result == 0) {
				*permission = 1;
			}
			else {
				*permission = 2;
			}
		}
		if (buf.st_size <= 0) {
			//  Return a truncated file as non-existent
			return 0;
		}
		//  it does exists
		return 1;
	}
	else {
		//  Non-zero return; no, it does not exists
		return 0;
	}
#else
	 //  Get the absolute pathname, or null if invalid.
	 fullName = realpath(name, NULL);
	 if (fullName == NULL) {
		 //  File does not exist
		 stringCopy(fullDssFilename, sizeofFilename, name, strlen(name));
	 }
	 else {
		 stringCopy(fullDssFilename, sizeofFilename, fullName, strlen(fullName));
		 free(fullName);
	 }

	 //  Determine if the file exists
	 buf.st_size = 0;

	result = stat64(fullDssFilename, &buf );
	//  _stat throws an error if the file does not exist - reset it.
	if (result == 0) {
		//  Get the file permission
		result = access(fullDssFilename, 06);
		if (result == 0) {
			//  Normal read/write permission
			*permission = 0;
		}
		else {
			//  Read permission or no access
			result = access(fullDssFilename, 04);
			if (result == 0) {
				*permission = 1;
			}
			else {
				*permission = 2;
			}
		}
		if (buf.st_size <= 0) {
			//  Return a truncated file as non-existent
			return 0;
		}
		//  it does exists
		return 1;
	}
	else {
		//  Non-zero return; no, it does not exists
		return 0;
	}
#endif
}

