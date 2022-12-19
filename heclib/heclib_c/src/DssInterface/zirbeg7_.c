#include <string.h>

#include "heclib7.h"
#include "heclib6.h"
#include "hecdssInternal.h"
#include "hecdssFort.h"


//  For Compatibility purposes only
//  Avoid using

void zirbeg7_ (int *julianStart, char *ePart, int *iyear, int *imonth,
			   int *iday, int *blockSize, int *minblk, int *incblk, slen_t lenEpart)
{
	int flag;
	int ierror;
	int julian;

	//  Get the block length

	flag = 1;
	ierror = ztsGetStandardInterval(7, blockSize, ePart, strlen(ePart), &flag);
	if (ierror == -1) {
		*blockSize = 0;
		return;
	}
	if (*blockSize >= 0) {
		//  Error
		*blockSize = 0;
		return;
	}
	//  Convert the block size to positive
	*blockSize = - (*blockSize);

	//  Now get the start of the block
	julian = ztsIrregGetBlockStart(*julianStart, *blockSize);

	//  And convert it to year, month, day
	jliymd_(&julian, iyear, imonth, iday);
	*minblk = 0;
	*incblk = 0;
}

