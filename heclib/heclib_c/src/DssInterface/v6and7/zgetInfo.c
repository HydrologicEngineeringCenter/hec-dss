
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "hecdssInternal.h"



//  DEPRECIATED - USE zgetRecordSize INSTEAD
//  This function is here only for version 6 compatibility
//
int zgetInfo7(long long *ifltab, const char *pathname, int *ibuff)
{

	int status;
	int jul;
	int isecs;
	long long ilarge;
	long long *info;

	/*
	ibuff[0] = data type
	ibuff[1] = version number (number of writes)
	ibuff[2] = expansion number (number of time expanded)
	ibuff[3] = expansion flag
	ibuff[4] = compression
	ibuff[5] = precision
	ibuff[6] = last written date in julian (since 1900)
	ibuff[7] = last written time in seconds past midnight
	ibuff[8,9,10,11] = last write program (16 characters long) = zdssVals.numberProgram
	*/

	if (zmessageLevel(ifltab, MESS_METHOD_UTILITY_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		zmessageDebug(ifltab, DSS_FUNCTION_other_ID, " Enter zgetInfo (old), Pathname: ", pathname);
	}

	status = zreadInfo(ifltab, pathname, 0);
	if (status != STATUS_RECORD_FOUND) {
		return status;
	}


	// existing record
	if (status == STATUS_RECORD_FOUND) {
		info = (long long *)ifltab[zdssKeys.kinfo];
		i8toi4(info[zdssInfoKeys.kinfoTypeVersion],  &ibuff[0], &ibuff[1]);
		i8toi4(info[zdssInfoKeys.kinfoExpansion], &ibuff[2], &ibuff[3]);
		//  Time in mills (since 01Jan1970)
		ilarge = info[zdssInfoKeys.kinfoLastWriteTime];
		ilarge = (ilarge/1000);
		jul = (int)(ilarge/ SECS_IN_1_DAY);
		isecs = (int)(ilarge - (((long long)jul) * SECS_IN_1_DAY));
		ibuff[6] = jul + JULIAN_01JAN1970;
		ibuff[7] = isecs;
		charLong(&info[zdssInfoKeys.kinfoProgram], &ibuff[8], zdssVals.numberProgram, zdssVals.numberProgram, 0, 0);
	}
	return 0;
}

//  Fortran compatible interface
void zgetinfo7_ (long long *ifltab, const char *pathname, int *ibuff, int *status, size_t lenPathname)
{
	char *path;
	path = stringFortToC(pathname, lenPathname);
	*status = zgetInfo7(ifltab, path, ibuff);
	free(path);
}

