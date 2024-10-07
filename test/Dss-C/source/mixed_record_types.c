#include "heclib.h"
#include <math.h >

int writeDoublesTimeSeries(long long* ifltab, const char* path, const char* date, const char* time, int some_missing_data);
int writeSingleTimeSeries(long long* ifltab, const char* path, const char* date, const char* time, int some_missing_data);

#define NUM_TS_VALUES 20

int get_record_type(long long ifltab[], const char* path) {
	// read record details.
	zStructRecordBasics* rb = zstructRecordBasicsNew(path);
	int	status = zgetRecordBasics(ifltab, rb);
	if (status != STATUS_OKAY) {
		return status;
	}
	int rval = rb->recordType;
	zstructFree(rb);
	
	return rval;
}

int check_values_compare(long long ifltab[], const char* path) {
	zStructTimeSeries* tss = zstructTsNew(path);
	int retrieveFlag = -1; // Trim data
	int status = ztsRetrieve(ifltab, tss, retrieveFlag, 1, 0);

	if (tss->numberValues != NUM_TS_VALUES) {
		printf("Expected  %d numberValues, found %d \n", NUM_TS_VALUES, tss->numberValues);
		return -1;
	}
	for (size_t i = 0; i < NUM_TS_VALUES; i++)
	{
		double d = 0;
		if (tss->floatValues) {
			d = tss->floatValues[i];
		}
		else if (tss->doubleValues) {
			d = tss->doubleValues[i];
		}
		double diff = fabs(d - i);
		if (diff > 0.001) {
			printf("inside time-series data Expected %d, found %.2f \n", (int)i, d);
			return -1;
		}
	}
}


/// <summary>
/// Writes doubles to a time-series with missing data
/// Writes floats to a time-series filling in the missing data.
/// </summary>
/// <returns></returns>
int write_ts_mixed(int dssVersion, int writeDoublesFirst) {
	long long ifltab[250];
	const char* dssFileName = "test_create_mixed_record_types.dss";

	deleteFile(dssFileName);
	int status = 0;
	if (dssVersion == 7) {
		status = hec_dss_zopen(ifltab, dssFileName);
	}
	else if (dssVersion == 6) {
		status = zopen6(ifltab, dssFileName);
	}
	if (status != STATUS_OKAY) {
		return status;
	}
	const char* date = "01Jun2024";
	const char* time = "1200";

	const char* path = "//GAPT_DAM/FLOW-LOCAL/01Jun2026/1Hour/GAPT_HMS_FORECAST/";
	
	if (writeDoublesFirst) {
		status = writeDoublesTimeSeries(ifltab, path, "01Jun2026", time, 1); // write doubles with gaps
		if (status != STATUS_OKAY) {
			return status;
		}
		status = writeSingleTimeSeries(ifltab, path, "01Jun2026", time, 0); // write floats without gaps
		if (status != STATUS_OKAY) {
			return status;
		}
	}
	else { // write floats first
		status = writeSingleTimeSeries(ifltab, path, "01Jun2026", time, 1); // write floats with gaps
		if (status != STATUS_OKAY) {
			return status;
		}
		if (!writeDoublesFirst) {
			status = writeDoublesTimeSeries(ifltab, path, "01Jun2026", time, 0); // write doubles with gaps
			if (status != STATUS_OKAY) {
				return status;
			}
		}
	}
	
	printf("\n '%s' record type: %d\n", path, get_record_type(ifltab, path));
	status = check_values_compare(ifltab, path);
	zclose(ifltab);
	return status;
}

int test_mixed_record_types() {

	int status = write_ts_mixed(7,0);
	if (status != STATUS_OKAY) {
		return status;
	}

	return status;

}

int writeDoublesTimeSeries(long long* ifltab, const char* path, const char* date, const char* time, int some_missing_data) {
	double dvalues[NUM_TS_VALUES];
	for (int i = 0; i < NUM_TS_VALUES; i++) {
		if (i % 3 ==0 && some_missing_data) {
			dvalues[i] = UNDEFINED_DOUBLE;
		}
		else {
			dvalues[i] = (double)i;
		}
	}
	zStructTimeSeries* tss1 = zstructTsNewRegDoubles(path, dvalues, NUM_TS_VALUES, date, time, "cfs", "Inst-Val");
	int storageFlagReplace = 1;
	int status = ztsStore(ifltab, tss1, storageFlagReplace);
	zstructFree(tss1);
	return status;
}
int writeSingleTimeSeries(long long* ifltab, const char* path, const char* date, const char* time, int some_missing_data) {
	float dvalues[NUM_TS_VALUES];
	for (int i = 0; i < NUM_TS_VALUES; i++) {
		if (i % 3 == 0  && some_missing_data) {
			dvalues[i] = UNDEFINED_FLOAT;
		}
		else {
			dvalues[i] = (float)i;
		}
	}
	zStructTimeSeries* tss1 = zstructTsNewRegFloats(path, dvalues, NUM_TS_VALUES, date, time, "cfs", "Inst-Val");
	int storageFlagReplace = 1;
	int status = ztsStore(ifltab, tss1, storageFlagReplace);
	zstructFree(tss1);
	return status;
}

