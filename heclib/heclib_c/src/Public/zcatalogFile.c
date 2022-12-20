#include <string.h>
#include <fcntl.h>

#ifdef _MSC_VER
#include <io.h>
#include <errno.h>
#else
#include <unistd.h>
#include <stdint.h>
#endif

#include "zdssKeys.h"
#include "zdssMessages.h"
#include "heclib.h"
#include "zerrorCodes.h"
#include "fortran_string_len_size.h"

/**
*  Function:	zcatalogFile
*
*  Use:			Public
*
*  Description:	Writes a list of pathnames in a DSS file to an (external) file
*
*  Declaration: int zcatalogFile(long long *ifltab, const char *catalogFilename, int boolSorted, const char *pathWithWildChars);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				const char *catalogFilename
*					The name of the file to write the list of pathnames to.  The file does not need to exist.
*					To write to the default name (file.dsc), then set this to null.
*
*				int boolSorted
*					Sort the pathname list using the pathname parts in the following order "ABCFED".  Sorting takes considerable
*					more resources, so only use if you need to.
*
*				const char *pathWithWildChars
*					Either null (for ignore) or a String that represents a pathname with wild characters represented
*					by a star (*) to match any string in the pathname part.  Wild characters can only be at the beginning or end of a part,
*					not inside of a string.  An example is a C part with "*Flow*" , which
*					will match all pathnames that have "Flow" anywhere in the C part, such as "Flow", "Inflow", "Outflow-Reg", etc.
*					A part such as "Flow*Reg" is not legal.  A null (//) will only match a null, where only a star (*) will match all.
*
*
*
*	Returns:	int numberCataloged
*					Number of pathnames in the file (a positive number)
*					or, if a negative number, then errorCode for error
*					errorCode may be decoded by function zerrorDecode()
*
*
*	Remarks:	For both DSS Version 6 and DSS Version 7 file.
*					This is very similar to function zcatalogToFile, except here, you just
*					provide the name of the DSS File.
*					This function is usually used when the calling function does not have many resources.
*
*	See Also:	zcatalog(), zcatalogToFile()
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zcatalogFile(long long *ifltab, const char *catalogFilename, int boolSorted, const char *pathWithWildChars)
{
	char catalogName[256];
	size_t len;
	int catalogHandle;
	int status;


	if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		if (catalogFilename) {
			zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "zcatalogFile, Catalog File:  ", catalogFilename);
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "zcatalogFile, No file name given ", "");
		}
		zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "Sort Catalog:  ", boolSorted);
		if (pathWithWildChars) {
			zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "Pathname with wild chars:  ", pathWithWildChars);
		}
		else {
			zmessageDebug(ifltab, DSS_FUNCTION_zcatalog_ID, "No Pathname with wild chars:  ", "");
		}
	}


	if (!catalogFilename) {
		if (zgetVersion(ifltab) == 7) {
			charLong((void *)ifltab[zdssKeys.kfullFilename], catalogName, 0, sizeof(catalogName), 0, 0);
			len = (int)strlen(catalogName);
		}
		else {
			zinquireChar(ifltab, "name", catalogName, sizeof(catalogName), &status);
			len = trimLengthLen(catalogName, sizeof(catalogName));
			if (len > 0) {
				catalogName[len] = '\0';
			}
		}
	}
	else {
		len = copyAndTrim(catalogName, sizeof(catalogName), catalogFilename, strlen(catalogFilename));
	}

	if (len > 5) {
		if (!strncmp(&catalogName[len-4], ".dss", 4)) catalogName[len-1] = 'c';
	}

#ifdef _MSC_VER
	catalogHandle = zopenFile(catalogName, (_O_RDWR | _O_CREAT | _O_TEXT | _O_TRUNC));
#else
	catalogHandle = zopenFile(catalogName, (O_RDWR | O_CREAT | O_TRUNC));
#endif
	if (catalogHandle <= 0) {
		if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_TERSE)) {
			zmessage2(ifltab, "Unable to open catalog file, file name: ", catalogName);
			perror("Catalog file error message ");
		}
		return zerrorProcessing(ifltab, DSS_FUNCTION_zcatalog_ID, zdssErrorCodes.UNABLE_TO_ACCESS_FILE,
								status, 0, zdssErrorSeverity.WARNING, "", catalogName);
	}

	if (boolSorted) {
		status = zcatInternalSort (ifltab, pathWithWildChars, (zStructCatalog *)0, catalogHandle, 0, 0);
	}
	else {
		status = zcatalogInternal (ifltab, pathWithWildChars, (zStructCatalog *)0, catalogHandle, 0, 0, 0, 0);
	}
	if (catalogHandle) {
		closeFile(catalogHandle);
	}
	if (zmessageLevel(ifltab, MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		 zmessageDebugInt(ifltab, DSS_FUNCTION_zcatalog_ID, "Exit zcatalogFile, status ", status);
	 }
	return status;
}


void zcatalogfile7_(long long *ifltab, const char *catalogFilename, int *boolSorted, const char *pathWithWildChars, int *status,
	slen_t lencatalogFilename, slen_t lenpathWithWildChars)
{
	char *filename;
	char *path;

	filename = stringFortToC(catalogFilename, lencatalogFilename);

	if (pathWithWildChars) {
		path = stringFortToC(pathWithWildChars, lenpathWithWildChars);
		*status = zcatalogFile(ifltab, filename, *boolSorted, path);
		free(path);
	}
	else {
		*status = zcatalogFile(ifltab, filename, *boolSorted, (const char *)0);
	}

	free(filename);
}

