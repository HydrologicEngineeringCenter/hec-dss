#include "heclib.h"

int testNoDates(int version)
{

	char* dssFileName = "test_no_dates_12_values.dss";
	deleteFile(dssFileName);

	long long ifltab[250];
	int status;
	if (version == 6)
		status = zopen6(ifltab, dssFileName);
	else
		status = hec_dss_zopen(ifltab, dssFileName);

	if (status != STATUS_OKAY) return status;

	// create simple record with 12 values
	double data[12];
	data[0] = 1.0;
	data[1] = 2.0;
	data[2] = 3.0;
	data[3] = 4.0;
	data[4] = 5.0;
	data[5] = 6.0;
	data[6] = 7.0;
	data[7] = 8.0;
	data[8] = 9.0;
	data[9] = 10.0;
	data[10] = 11.0;
	data[11] = 12.0;
	int size = 12;
	char pathname[150];
	sprintf(pathname, "/TEST/TEST/NONE//1Month/TEST/");

//	zStructTimeSeries* tss1 = zstructTsNewRegDoubles(pathname, data, 12, "01Oct1921", "0000", "kPa", "PER-AVER");
	zStructTimeSeries* tss1 = zstructTsNewRegDoubles(pathname, data, 12, "30Sep1921", "2400", "kPa", "PER-AVER");
	status = ztsStore(ifltab, tss1, 0);
	zstructFree(tss1);
	if (status != STATUS_OKAY)
	{
		printf("\nerror saving %s", pathname);
		zclose(ifltab);
		return status;
	}
	zStructTimeSeries* tss = zstructTsNew(pathname);
	status = ztsRetrieve(ifltab, tss, -1, 1, 0);
	printf("tss->numberValues = %d\n", tss->numberValues);
	int numValues = tss->numberValues;

	zstructFree(tss);

	if (numValues != 12) {
		printf("\nExpected 12 values, only found %d", numValues);
		zclose(ifltab);
		return -1;
	}

	if (status != STATUS_OKAY)
	{
		printf("\nerror reading %s", pathname);
	}
	zclose(ifltab);
	return status;
}
