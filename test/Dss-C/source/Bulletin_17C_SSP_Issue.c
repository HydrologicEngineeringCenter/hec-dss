#include "stdio.h"
#include "string.h"
#include "math.h"

#include "heclib.h"
#include "hecdss7.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"


int Bulletin_17C_SSP_Issue()
{
	long long ifltab[250];
	int i, status, rval;
	char cdate[13], ctime[10];
	zStructTimeSeries *tss;
	int retrieveFlag = 0;
	int retrieveDoublesFlag = 0;
	int boolRetrieveQualityNotes = 0;
	rval = 0;
	memset(ifltab, 0, sizeof(ifltab));
	//  Open the DSS file; Create if it doesn't exist
	
	status = hec_dss_zopen(ifltab, "Bulletin_17C_Examples.dss");
	if (status != STATUS_OKAY) return status;
	  tss = zstructTsNew("/Santa Cruz River/Lochiel/FLOW-ANNUAL PEAK//IR-CENTURY/USGS/");
	  tss->boolRetrieveAllTimes = 1;

	//zset("mlvl", "", 15);
	status = ztsRetrieve(ifltab, tss, retrieveFlag,retrieveDoublesFlag, boolRetrieveQualityNotes);

	printf("\n tss->numberValues = %d", tss->numberValues);

	if (tss->numberValues != 65)
	{
		printf("\n\nnn Expected 65 values \nnnn ");
		rval = -1;
	}
	//  Print out.  (Compute time for each ordinate, values are floats)
	 
	for (int i = 0; i < tss->numberValues; i++) {
		getDateAndTime(tss->times[i], tss->timeGranularitySeconds, tss->julianBaseDate,
			cdate, sizeof(cdate), ctime, sizeof(ctime));
		printf("Oridnate %d, for %s, %s, value is %f\n", i, cdate, ctime, tss->floatValues[i]);
	}

	zstructFree(tss);
	zclose(ifltab);
	return rval;


}
