#include "hecdssInternal.h"
#include "zdssKeys.h"


/**
*  Function:	zgetVersion
*
*  Use:			Public   
*
*  Description:	A quick, small funtion to get the major version number of a (opened) DSS file.
* 
*  Declaration: int zgetVersion(long long *ifltab);
*
*  Parameters:	
*				long long *ifltab - the file table of the opened DSS file
*
*	Returns:	7:  A DSS version 7 file
*				6:  A DSS version 6 file
*				0:  File not opened or error
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zgetVersion(long long *ifltab)
{
	
#ifndef __sun__
	return (int)ifltab[0];
#else 
	int vers;
	int v2;
	i8toi4(ifltab[0], &vers, &v2);
	if (v2 != 0)
		return v2;
	return vers;
#endif
}

int getVersFromChar(char version)
{
	//  Valid characters are A-Z, a-z
	//	A ==1, B ==2, a==27, b==28
	int iver = 0;
	if ((int)version < 65) {
		if (((int)version > 47) && ((int)version < 58)) {
			iver = (int)version - 48;
			return iver;
		}
		else {
			return 0;
		}
	}
	if ((int)version < 91) {
		iver = (int)version - 64;
	}
	if ((int)version > 91) {
		iver = (int)version - 96 + 26;
	}
	return iver;
}


int zgetFullVersion(long long *ifltab)
{
	char cversion[9];
	long long *fileHeader;

	//  For example DSS Version "7-BG"
	//  Valid characters are A-Z, a-z  A == 1, B == 2, a == 27, b == 28
	int version;		//	70207
	int majorVersion;	//  7
	int subVersion;		//	B	= 02
	int minorVersion;	//	G	= 07

	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	charInt((void *)&fileHeader[zdssFileKeys.kversion], cversion, 5, sizeof(cversion), 0, 1, 0);

	majorVersion = getVersFromChar(cversion[0]);
	subVersion = getVersFromChar(cversion[2]);
	minorVersion = getVersFromChar(cversion[3]);

	version = (majorVersion * 10000) + (subVersion * 100) + minorVersion;
	return version;
}




