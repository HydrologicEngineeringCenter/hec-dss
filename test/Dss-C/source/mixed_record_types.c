#include "heclib.h"


int writeDoublesTimeSeries(long long* ifltab, const char* path, const char* date, const char* time);
int writeSingleTimeSeries(long long* ifltab, const char* path, const char* date, const char* time);


int test_mixed_record_types() {
	long long ifltab[250];
  const char* dssFileName = "test_mixed_record_types.dss";

  deleteFile(dssFileName);
	int status = hec_dss_zopen(ifltab, dssFileName);
	if (status != STATUS_OKAY) {
		return status;
	}
	const char* date = "01Jun2024";
	const char* time = "1200";

	const char* path = "//GAPT_DAM/FLOW-LOCAL/01Jun2024/1Hour/GAPT_HMS_FORECAST/";

	status = writeSingleTimeSeries(ifltab, path, date, time);
	if (status != STATUS_OKAY) {
		return status;
	}
	status = writeDoublesTimeSeries(ifltab, path, date, time);


  // re-write record (singles)

  // read data to check

	return status;
}

int writeDoublesTimeSeries(long long* ifltab, const char* path, const char* date, const char* time) {
	double dvalues[20];
	for (int i = 0; i < 20; i++) {
		dvalues[i] = (double)i;
	}
	zStructTimeSeries* tss1 = zstructTsNewRegDoubles(path, dvalues, 20, date, time, "cfs", "Inst-Val");
	int storageFlagReplace = 0;
	int status = ztsStore(ifltab, tss1, storageFlagReplace);
	zstructFree(tss1);
	return status;
}
int writeSingleTimeSeries(long long* ifltab, const char* path, const char* date, const char* time) {
	float dvalues[20];
	for (int i = 0; i < 20; i++) {
		dvalues[i] = (float)i;
	}
	zStructTimeSeries* tss1 = zstructTsNewRegFloats(path, dvalues, 20, date, time, "cfs", "Inst-Val");
	int storageFlagReplace = 0;
	int status = ztsStore(ifltab, tss1, storageFlagReplace);
	zstructFree(tss1);
	return status;
}