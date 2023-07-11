
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "hecdssInternal.h"



//
//
int zputInfo (long long *ifltab, const char *pathname, int *ibuff)
{

	int status;
	int numberInfo;
	int idum;
	int itemp;
	long long info[100];

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


	status = zcheck(ifltab, pathname);
	if (zisError(status)) {
		return zerrorUpdate(ifltab, status, DSS_FUNCTION_other_ID);
	}


	if (status == STATUS_RECORD_FOUND) {
		//  Read in the info block
		numberInfo = zdssVals.infoSize + numberLongsInBytes((int)ifltab[zdssKeys.klenLastPath]);
		if (numberInfo > 100)
			numberInfo = 100;
		status = zget(ifltab, ifltab[zdssKeys.kaddInfoLastPath], (int *)info, numberInfo, 2);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_other_ID);
		}

			//  Type, Version in loc 0
		info[zdssInfoKeys.kinfoTypeVersion] = i4toi8(ibuff[0], ibuff[1]);
		i8toi4(info[zdssInfoKeys.kinfoExpansion], &itemp, &idum);
		if (ibuff[2] == 0) {
			ibuff[2] = itemp;
		}
		info[zdssInfoKeys.kinfoExpansion] = i4toi8(ibuff[2], ibuff[3]);
		info[zdssInfoKeys.kinfoLastWriteTime] = ((long long)(ibuff[6] - JUL_01JAN1970) * SECS_IN_1_DAY) + (long long)ibuff[7];
		charLong(&ibuff[8], &info[zdssInfoKeys.kinfoProgram], zdssVals.numberProgram, zdssVals.numberProgram, 0, 0);

		status = zput(ifltab, ifltab[zdssKeys.kaddInfoLastPath], (int *)info, numberInfo, 2);
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_other_ID);
		}
	}
	return status;
}

//  Fortran compatible interface
void zputinfo7_ (long long *ifltab, const char *pathname, int *ibuff, int *status, size_t lenPathname)
{
	char *path;

	path = stringFortToC(pathname, lenPathname);
	*status = zputInfo (ifltab, path, ibuff);
	free(path);
}

