#include <stdio.h>
#include "heclib.h"

 int main()
{
	long long ifltab[250];
	zStructTimeSeries *tss1;
	float fvalues[1];
	int status;

	fvalues[0] = 3.14159;

	status = hec_dss_zopen(ifltab, "ts_write.dss");
	if (status != STATUS_OKAY)
	{
		printf("\nError:  Yikes.. bad news with zopen(ts_write.dss)");
		return status;
	}
	printf("\ncalling zstructTsNewRegFloats(...) ");
	tss1 = zstructTsNewRegFloats("/Basin/Location/Flow//1Hour/C Test/", fvalues, 1,
		"21Jan2001", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	zstructFree(tss1);
	if (status != STATUS_OKAY) 
		 return status;

	zclose(ifltab);


	return status;
}
