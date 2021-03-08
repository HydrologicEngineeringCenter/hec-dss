#include "heclib.h"
#include <math.h>
#include <string.h>
int main()
{
	long long ifltab[250];
	zStructTimeSeries *tss1, *tssOut;
	double dvalues[3];
	int status, i, count;
	int itimes[3];
	char cdate[13], ctime[10];

	memset(ifltab, 0, sizeof(ifltab));
	dvalues[0] = 3.14159*1.0;
	dvalues[1] = 3.14159*2.0;
	dvalues[2] = 3.14159*3.0;

	itimes[0] = 1440 * 0;
	itimes[1] = 1440 * 1;
	itimes[2] = 1440 * 2;

	count = 3;
	status = zopen(ifltab, "ts_write_irregular.dss");
	if (status != STATUS_OKAY)
	{
		printf("\nError:  Yikes.. bad news with zopen(ts_write_irregular.dss)");
		return status;
	}
	printf("\ncalling zstructTsNewIrregDoubles(...) ");
	tss1 = zstructTsNewIrregDoubles("/Basin/Location/Flow//~1Day/ts_write_irregular/",
		dvalues, count, itimes, MINUTE_GRANULARITY, "20April2012", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	zstructFree(tss1);
	if (status != STATUS_OKAY)
		return status;


	tssOut = zstructTsNewTimes("/Basin/Location/Flow//~1Day/ts_write_irregular/", "19April2012", "2400", "01July2013", "2400");
	status = ztsRetrieve(ifltab, tssOut, 0, 2, 0);
	if (status != STATUS_OKAY) return status;
	//  Print out (values returned as doubles)
	if (tssOut->numberValues != count)
	{
		printf("\nError expected %d values, found %d values.", count, tssOut->numberValues);
		return -1;
	}
	for (i = 0; i < tssOut->numberValues; i++) {
		getDateAndTime(tssOut->times[i], tssOut->timeGranularitySeconds, tssOut->julianBaseDate,
			cdate, sizeof(cdate), ctime, sizeof(ctime));

		if (fabs(tssOut->doubleValues[i] - 3.14159*(i + 1)) > 0.01)
		{
			printf("\nError expected: %f, found %f ", 3.14159*(i + 1), tssOut->doubleValues[i]);
			return -1;
		}
		if ( i==0 && strncmp(cdate,"19Apr2012",9) != 0)
		{
			printf("\nError w/Date expected: %s, found %s ", "19Apr2012",cdate);
			return -1;
		}
		if (i == 1 && strncmp(cdate, "20Apr2012", 9) != 0)
		{
			printf("\nError w/Date expected: %s, found %s ", "19Apr2012", cdate);
			return -1;
		}
		if (i == 2 && strncmp(cdate, "21Apr2012", 9) != 0)
		{
			printf("\nError w/Date expected: %s, found %s ", "19Apr2012", cdate);
			return -1;
		}


		printf("Oridnate %d, for %s, %s, value is %f\n", i, cdate, ctime, tssOut->doubleValues[i]);
	}



	zclose(ifltab);


	return status;
}
