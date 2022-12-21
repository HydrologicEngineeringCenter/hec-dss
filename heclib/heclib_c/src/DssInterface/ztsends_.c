#include "heclib.h"
#include "heclib6.h"
#include "hecdssInternal.h"



//  Depreciated.  Use ztsGetDateRange() instead

//      SUBROUTINE ztsends6(IFLTAB, CPATH, ISEARCH, JULS, ISTIME,
//     * JULE, IETIME, LFOUND)

void ztsends_(long long *ifltab, const char* pathname, int *isearch,
			  int *juls, int *istime, int *jule, int *ietime,
			  int *boolFound, size_t lenPathname)
{
	int status;
	int startSeconds;
	int endSeconds;

	if (zgetVersion(ifltab) == 6) {
		ztsends6_((int *)ifltab, pathname, isearch, juls, istime, jule, ietime,
			boolFound, lenPathname);
	}
	else {
		status = ztsGetDateTimeRange(ifltab, pathname, 1, juls, &startSeconds, jule, &endSeconds);
		if (status == STATUS_RECORD_FOUND) {
			*boolFound = 1;
			*istime = startSeconds/60;
			*ietime = endSeconds/60;
		}
		else {
			*boolFound = 0;
		}
	}

}

