#include "hecdss7.h"
#include "hecdssFort.h"


/**
*  Function:	zgetVersion (Fortran)
*
*  Use:			Public, Fortran compatible
*
*  Description:	A quick, small function to get the major version number of a (opened) DSS file.
*
*  Declaration: void zgetversion_(int *ifltab, int *version);
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



void zgetversion_(int *ifltab, int *version)
{
	*version = zgetVersion((void *)ifltab);
}



