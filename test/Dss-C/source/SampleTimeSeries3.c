#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "hecdss7.h"
#include "zStructTimeSeries.h"
#include "hecdssInternal.h"
#include "TestDssC.h"




int sampleTimeSeries3()
{
	long long ifltab[250];
	zStructTimeSeries *tss1, *tss2;
	double dvalues[200];
	int quality[200];
	int i;
	int status;
	int zero = 0;

	for (i=0; i<200; i++) {
		dvalues[i] = (double)i;
	}

	for (i=0; i<200; i++) {
		dvalues[i] = (double)i;
		//  set evens to 3, odds to 5
		if ((i & 1) == 0) {
			quality[i] = 3;
		}
		else {
			quality[i] = 5;
		}
	}

	tss1 = zstructTsNewRegDoubles("/Basin/Location/Flow//1Hour/Quality/", dvalues, 200,"20Jan2010", "2400", "cfs", "Inst-Val");
	tss1->quality = quality;

	status = hec_dss_zopen(ifltab, "Sample7.dss");
	status = ztsStore(ifltab, tss1, 0);

	tss2 = zstructTsNew("/Basin/Location/Flow//1Hour/Quality/");
	status = ztsRetrieve(ifltab, tss2, 0, 0, 1);

	zclose(ifltab);


	zstructFree(tss1);
	zstructFree(tss2);

	return 0; 
}

