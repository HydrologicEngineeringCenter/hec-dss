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

	
	status = ztsGetDateTimeRange(ifltab, pathname, 1, juls, &startSeconds, jule, &endSeconds);
	if (status == STATUS_RECORD_FOUND) {
		*boolFound = 1;
		*istime = startSeconds / SECS_IN_1_MINUTE;
		*ietime = endSeconds / SECS_IN_1_MINUTE;
	}
	else {
		*boolFound = 0;
	}
	

}

