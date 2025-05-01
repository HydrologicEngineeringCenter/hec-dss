#include <stdlib.h>
#include <string.h>

#ifdef _MSC_VER
#include <tchar.h>
#else
#include <libgen.h>
#endif


#include "heclib7.h"

/**
*  Function:	getFileFromPath
*
*  Use:			Public
*
*  Description:	Parses a file name and returns the name without the path (but includes extension)
*
*  Declaration: char *getFileFromPath (char *filename, size_t sizeOfFilename, const char *fullpath)
*
*  Parameters:
*				char *filename:  The file name only (with extension)  This should be declared as
*					char filename[_MAX_FNAME];
*
*				size_t sizeOfFilename:  The size of the char array filename.  (should be _MAX_FNAME)
*
*				const char *fullpath:  The path/file name to parse.
*
*
*	Returns:	char *filename or NULL if invalid file name.
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

 char *getFileFromPath (char *filename, size_t sizeOfFilename, char *fullpath)
 {
#ifdef _MSC_VER
   char drive[_MAX_DRIVE];
   char dir[MAX_FILENAME_LENGTH];
   char *fname;
   char ext[_MAX_EXT];
   errno_t err;
   size_t len;

	err = _splitpath_s( fullpath, drive, _MAX_DRIVE, dir, MAX_FILENAME_LENGTH, filename,
                       sizeOfFilename, ext, (size_t)_MAX_EXT );
	if (err) return NULL;

	len = strnlen_hec(ext, _MAX_EXT);
	stringCat(filename, MAX_FILENAME_LENGTH, ext, len);
	fname = filename;

#else
	 char *fname;
	 fname = basename(fullpath);
	 if (fname) {
		stringCopy(filename, sizeOfFilename, fname, strlen(fname));
		fname = filename;
	 }
#endif

	return fname;
}

