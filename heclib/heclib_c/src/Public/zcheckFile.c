#include "heclib7.h"
#include "heclib6.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "zprogress.h"
#include "hecdssInternal.h"


/**
*  Function:	zcheckFile
*
*  Use:			Public
*
*  Description:	Thorough checking of the integrity of all components and addresses in a DSS file.
*					Combines all of the file checking function into one.
*					Intent is to look for any damage within the DSS file
*					(Note: Resource intensive; most intensive function in DSS.)
*
*  Declaration: int zcheckFile(long long *ifltab);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*	Returns:	int status
*					STATUS_OKAY for no errors.
*					> 0 for number of errors in the file detected (some errors will generate multiple errors)
*					< 0 errorCode for severe error
*
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int zcheckFile(long long *ifltab)
{

	int istat;
	long long *fileHeader;
	char ctemp[5] = {0};

	/*  Future update
	istat = zfileHeaderCheck(ifltab, 3);
	if (istat != 0)
		return istat;
	zinquireChar(ifltab, "error", ctemp, sizeof(ctemp), &istat);
	if (istat != 0)
		return istat;



	*/


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	zresetProgress(zhandle(ifltab), fileHeader[zdssFileKeys.knumberRecords]);
	zprogress.handle = zhandle(ifltab);

	istat = zcheckLinks(ifltab);
	if (istat != STATUS_OKAY)
		return istat;
	zinquireChar(ifltab, "error", ctemp, sizeof(ctemp), &istat);
	if (istat != STATUS_OKAY)
		return istat;

	zresetProgress(zhandle(ifltab), fileHeader[zdssFileKeys.knumberRecords]);
	zprogress.handle = zhandle(ifltab);

	istat = zcheckPathnames(ifltab);
	if (istat != STATUS_OKAY)
		return istat;
	zinquireChar(ifltab, "error", ctemp, sizeof(ctemp), &istat);
	if (istat != STATUS_OKAY)
		return istat;

	zresetProgress(zhandle(ifltab), fileHeader[zdssFileKeys.knumberRecords]);
	zprogress.handle = zhandle(ifltab);

	istat = zcheckPathnameBins(ifltab);
	if (istat != STATUS_OKAY)
		return istat;
	zinquireChar(ifltab, "error", ctemp, sizeof(ctemp), &istat);
	if (istat != STATUS_OKAY)
		return istat;

	zresetProgress(zhandle(ifltab), fileHeader[zdssFileKeys.knumberRecords]);
	zprogress.handle = zhandle(ifltab);

	istat = zcheckHashTable(ifltab);
	if (istat != STATUS_OKAY)
		return istat;
	zinquireChar(ifltab, "error", ctemp, sizeof(ctemp), &istat);
	if (istat != STATUS_OKAY)
		return istat;

	return istat;
}

void zcheckfile_(long long *ifltab, int *errors)
{
	*errors = zcheckFile(ifltab);
}

