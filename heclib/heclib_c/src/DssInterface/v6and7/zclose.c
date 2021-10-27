#include "heclib7.h"
#include "hecdssFort.h"
#include "heclib6.h"

int zclose(long long *ifltab)
{
	if (zgetVersion(ifltab) == 6) {
		zclose6_(ifltab);
		return 0;
	}
	else {
		return zcloseInternal(ifltab, 0);
	}
}



