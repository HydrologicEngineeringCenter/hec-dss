#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "heclib.h"
#include "zdssKeys.h"
#include "zdssMessages.h"
#include "zdssVals.h"
#include "hecdssInternal.h"


/////////////////////////////
///////////////  Deprecated   Use ztsGetDateRange instead.
/////////////////////////////

int ztsinfo7_(long long *ifltab, const char *path,  int *firstJulian, int *firstMins,
			   int *lastJulian, int *lastMins, char *cunits, char *ctype,
			   int *lquality, int *ldouble, int *lfound,
			   size_t lenpath, size_t lencunits, size_t lenctype)
{

	/*
      Retrieve information about a time series record
      (both regular interval and irregular interval).
      The pathname must be valid (with a correct D part).

      Input:
         IFLTAB:  Working DSS array used in zopen6 call.
         CPATH:   Pathname of the info to be retrieved.

      Output:
         JULS:    The julian date of the first valid (non-missing)
                  data value in the record.
         ISTIME:  The time in minutes past midnight of this value
         JULE:    The julian date of the last valid (non-missing)
                  data value in the record.
         IETIME:  The time in minutes past midnight of this value.
         CUNITS:  Character string returning the units of the data.
                  CUNITS must be declared CHARACTER*8
         CTYPE:   Character string returning the type of the data
                  (e.g., PER-AVER).  CTYPE must be declared CHARACTER*8
         LQUAL:   Logical flag indicating whether quality values are
                  store with the data set.
         LDOUBLE: Logical flag indicating whether the data is stored
                  as double precision.
         LFOUND:  Logical flag indicating if the record exists.
                  If this is returned FALSE, all other output
                  is undefined.
	*/
	 zStructTimeSeries *tss;
	int istat;
	char *pathname;

	pathname = stringFortToC(path, lenpath);
	tss = zstructTsNew(pathname);
	free(pathname);
	istat = ztsRetrieve(ifltab, tss, -1, 0, 0);

	if (istat != STATUS_RECORD_FOUND) {
		*lfound = 0;
		zstructFree(tss);
		return istat;
	}

	*lfound = 1;

	if ((tss->dataType == DATA_TYPE_RTD) || (tss->dataType == DATA_TYPE_ITD)) {
		*ldouble = 1;
	}
	else {
		*ldouble = 0;
	}

	stringCToFort(cunits, lencunits,  tss->units);
	stringCToFort(ctype, lenctype,  tss->type);

	*firstJulian = tss->startJulianDate;
	*firstMins = tss->startTimeSeconds / 60;
	*lastJulian = tss->endJulianDate;
	*lastMins = tss->endTimeSeconds / 60;
	*lquality = 0;
	zstructFree(tss);
	return 0;
}

