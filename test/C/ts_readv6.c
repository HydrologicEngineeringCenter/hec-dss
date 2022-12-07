#include <stdio.h>
#include <math.h>
#include "heclib.h"

 int main()
{
	long long ifltab[250];
	zStructTimeSeries *tss1;
	float fvalues[1];
	int valueTime,i,status;
	char cdate[13], ctime[10];
	fvalues[0] = 3.14159;
	int trimData = -1; 
	int returnFloats = 1;
	int boolRetrieveQualityNotes = 0;
	float expected;

	status = hec_dss_zopen(ifltab, "v6-pc.dss");
	zsetMessageLevel(6, 17);
	if (status != STATUS_OKAY)
	{
		printf("\nError:  Yikes.. bad news with zopen(v6-pc.dss)");
		return status;
	}
	
	tss1 = zstructTsNew("/BOISE RIVER/LUCKYPEAK/STORAGE//1DAY/OBS/");
	status = ztsRetrieve(ifltab, tss1, trimData, returnFloats, boolRetrieveQualityNotes);
	printf("ztsRetrieve.status= %d ", status);
	if (status != STATUS_OKAY) 
		return status;
	//  Print out.  (Compute time for each ordinate, values are floats)
	if (tss1->numberValues < 4)
	{
		printf("\nError. expected more records in the time series. numberValues = %d", tss1->numberValues);
		return -1;
	}

	valueTime = tss1->startTimeSeconds;
	for (i = 0; i < tss1->numberValues; i++) {
		getDateAndTime(valueTime, SECOND_GRANULARITY, tss1->startJulianDate,
			cdate, sizeof(cdate), ctime, sizeof(ctime));
		printf("Oridnate %d, for %s, %s, value is %f\n", i, cdate, ctime, tss1->floatValues[i]);
		expected = i + 1;
		if (fabs(tss1->floatValues[i] - (i + 1)) > 0.01)
		{
			printf("Error, expected: %.2f  actual: %.2f\n", (float)(i + 1), tss1->floatValues[i] );
			return -1;
		}

		valueTime += 3600;  //  Increment to next hour
	}

	zstructFree(tss1);
	if (status != STATUS_OKAY) 
		 return status;

	zclose(ifltab);


	return status;
}
