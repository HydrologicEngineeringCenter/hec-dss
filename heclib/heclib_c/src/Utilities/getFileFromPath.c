#include <stdlib.h>
#include <string.h>

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
*					char filename[MAX_FILENAME_LENGTH];
*
*				size_t sizeOfFilename:  The size of the char array filename.  (should be MAX_FILENAME_LENGTH)
*
*				const char *fullpath:  The path/file name to parse.
*
*
*	Returns:	char *filename or NULL if invalid file name.
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
**/

 char *getFileFromPath (char *filename, size_t sizeOfFilename, const char *fullpath)
 {
	const char *base;
	const char *p;

	if (filename == NULL || fullpath == NULL || sizeOfFilename == 0) {
		return NULL;
	}

	//  Accept either separator, or mixed separators, to handle both Unix and Windows paths. 
	base = fullpath;
	if ((p = strrchr(base, '/'))  != NULL) base = p + 1;
	if ((p = strrchr(base, '\\')) != NULL) base = p + 1;

	stringCopy(filename, sizeOfFilename, base, strnlen_hec(base,MAX_FILENAME_LENGTH));
	return filename;
}
