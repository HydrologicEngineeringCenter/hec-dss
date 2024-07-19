
#include <stdio.h>

#include "heclib7.h"
#include "hecdssFort.h"
#include "zdssKeys.h"
#include "zprogress.h"
#include "hecdssInternal.h"

/**
*  Function:	zloadCache, copyFile
*
*  Use:			Private (Internal)
*
*  Description:	A simple function that just reads all of a file.  This causes MS Windows to load the file
*					into its cache and makes accesses much faster (as the file is in memory).
*					Optionally, the file can be copied to another file at the same time.
*
*  Declaration: int copyFile(long long *ifltab, int handleTo);
*				int zloadCache (long long *ifltab)
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.
*
*				int handleTo
*					If copying to another file, then this is the handle number to that file.
*					If just loading into cache, set this to zero (0).
*
*
*	Returns:	int status
*					The system status of the operation (not DSS status).  Returns zero if okay.
*
*
*
*	Author:			Bill Charley
*	Date:			2013
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/
int copyFile(long long *ifltab, int handleTo)
{

	int istat;
	int ihandle;
	int numberReads;
	int numberInts;
	int i;
	int j;
	long long iaddress;
	static int isize = 32768;
	long long space[32768];
	long long *fileHeader;


	fileHeader = (long long *)ifltab[zdssKeys.kfileHeader];
	ihandle = (int)ifltab[zdssKeys.khandle];
	numberReads = (int)(fileHeader[zdssFileKeys.kfileSize] / (long long) 32768) + 1;
	numberInts = isize * 2;
	iaddress = 0;
	zresetProgress(zhandle(ifltab), (long long)numberReads);

	for (i=0; i<numberReads; i++) {
		istat = zreadDisk (ihandle, 0, iaddress, space, numberInts);
		if (istat) {
		}
		if (handleTo > 0) {
			if ((i+1) == numberReads) {
				for (j=0; j<isize; j++) {
					if (space[j] == DSS_END_FILE_FLAG) {
						numberInts = (j + 1) * 2;
						break;
					}
				}
			}
			istat = zwriteDisk (handleTo, 0, iaddress, space, numberInts);
			if (istat) {
			}
		}
		iaddress += isize;
		zprogress.currentNumber = i;
		zprogress.handle = zhandle(ifltab);
		if (zprogress.interrupt) break;
	}

	return istat;
}

int zloadCache (long long *ifltab)
{
	return copyFile(ifltab, 0);
}

void zloadcache7_(long long *ifltab, int *istat)
{
	*istat = zloadCache(ifltab);
}

void zloadcache_(long long *ifltab, int *istat)
{	
	*istat = zloadCache(ifltab);
}
