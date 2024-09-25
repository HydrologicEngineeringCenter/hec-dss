#include <ctype.h>

#include "hecdss7.h"

/**
*  Function:	zpathnameCompare
*
*  Use:			Public
*
*  Description:	compare two pathnames, case insensitive (lengths the same)
*
*  Declaration: int zpathnameCompare(const char *pathname1, long long *lpathname2, size_t pathnameLength);
*
*  Parameters:	const char *pathname1:  The first pathname to compare
*
*				long long *lpathname2:  The second pathname to compare from the long long path bin or similar
*						On big endian machines, this will first be converted from long to a correct character string.
*
*				int pathnameLength:  The length of each pathname (Note:  both pathnames have
*					to be the same length, so check that first.
*
*  Returns:		int (boolean) same:		0 (zero) is not the same
*										1 (One) if the same
*
*  Remarks:		Case insensitive, may be either upper or lower, results will be the same
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zstringCompare(const char *string1, const char *string2, size_t length)
{
	int i;
	int i1;
	int i2;

	for (i = 0; i<(int)length; i++) {
		i1 = toupper(string1[i]);
		i2 = toupper(string2[i]);
		if (i1 != i2) {
			return 0;
		}
	}
	//  Made it - same strings
	return 1;
}

/// <summary>
/// compares two strings (must be same length)
/// </summary>
/// <param name="length">length of strings</param>
/// <returns></returns>
int zstringCompareSensitive(const char* string1, const char* string2, size_t length)
{
	int i;

	for (i = 0; i < (int)length; i++) {
		if( string1[i] != string2[i])
		return 0;
	}
	//  Made it - same strings
	return 1;
}


int zpathnameCompare(const char *pathname1, long long *lpathname2, size_t pathnameLength)
{
#ifdef _MSC_VER
	int i;
	int i1;
	int i2;
	char *pathname2 = (char *)lpathname2;

	for(i=0; i<(int)pathnameLength; i++) {
		i1 = toupper(pathname1[i]);
		i2 = toupper(pathname2[i]);
		if (i1 != i2) {
			return 0;
		}
	}
	//  Made it - same paths
	return 1;
#else
	int i;
	int i1;
	int i2;
	char pathname2[393];
	
	charLong(lpathname2, pathname2, pathnameLength, sizeof(pathname2), 0, 0);
	for (i = 0; i<(int)pathnameLength; i++) {
		i1 = toupper(pathname1[i]);
		i2 = toupper(pathname2[i]);
		if (i1 != i2) {
			return 0;
		}
	}
	//  Made it - same paths
	return 1;
#endif
}

