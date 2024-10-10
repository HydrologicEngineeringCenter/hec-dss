#include "heclib.h"
#include <math.h>

int writeDoublesTimeSeries(long long* ifltab, const char* path, const char* date, const char* time, int some_missing_data);
int writeDoubleIrregularTimeSeries(long long* ifltab, const char* path, const char* date, const char* time, int some_missing_data);
int writeFloatIrregularTimeSeries(long long* ifltab, const char* path, const char* date, const char* time, int some_missing_data);
int writeSingleTimeSeries(long long* ifltab, const char* path, const char* date, const char* time, int some_missing_data);



#define NUM_TS_VALUES 20

/// prints out all paths, returns non-zero if any records are floats  (we are expecting doubles)
int check_catalog_for_doubles(long long* ifltab) {
	zStructCatalog* catStruct = zstructCatalogNew();
	int status = zcatalog(ifltab, (const char*)0, catStruct, 1);
	if (status < 0) {
		printf("Error during catalog.  Error code %d\n", status);
		return status;
	}
	int rval = 0;
	for (int i = 0; i < catStruct->numberPathnames; i++)
	{
		zStructRecordBasics* recordBasics = zstructRecordBasicsNew(catStruct->pathnameList[i]);
		// zset("MLEV", "",17 );
		status = zgetRecordBasics(ifltab, recordBasics);

		printf("[%d] \"%s\" %d\n", i, catStruct->pathnameList[i], recordBasics->recordType);
		if (recordBasics->recordType == DATA_TYPE_RTS || recordBasics->recordType == DATA_TYPE_ITS) {
			printf(" <<< ERROR: expected Doubles\n");
			rval = -105;
		}
		zstructFree(recordBasics);
	}

	zstructFree(catStruct);
	return rval;
}

int check_values_compare(long long ifltab[], const char* path, int irregular) {
	zStructTimeSeries* tss = zstructTsNew(path);
	int retrieveFlag = -1; //
	int status = ztsRetrieve(ifltab, tss, retrieveFlag, 1, 0);
	char cdate[13], ctime[10];

	int rval = 0;

	if (tss->numberValues != NUM_TS_VALUES) {
		printf("Expected  %d numberValues, found %d \n", NUM_TS_VALUES, tss->numberValues);
		rval = -1;
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

		if (irregular) {
			getDateAndTime(tss->times[i], tss->timeGranularitySeconds, tss->julianBaseDate,
				cdate, sizeof(cdate), ctime, sizeof(ctime));
		}

		double diff = fabs(d - i);
		if (diff > 0.001) {
			if (irregular)
				printf("expected %d, found   %s %s, %f\n", (int)i, cdate, ctime, d);
			else
				printf("expected %d, found  %f\n", (int)i, d);
			rval = -2;
		}
	}
	return rval;
}


/// <summary>
/// Writes doubles to a time-series with missing data
/// Writes floats to a time-series filling in the missing data.
/// </summary>
/// <returns></returns>
int write_irregular_ts_mixed(long long* ifltab, int writeDoublesFirst) {
	int status = 1;
	const char* date = "30Jun2026";
	const char* time = "2300";

	char* path_float = "/ResSim//Flow//~1Hour/TSS-Floats/";
	char* path_double = "/ResSim//Flow//~1Hour/TSS-Doubles/";
	char* path;
	if (writeDoublesFirst) {
		path = path_double;
		status = writeDoubleIrregularTimeSeries(ifltab, path, date, time, 1); // write doubles with gaps
		if (status != STATUS_OKAY) {
			return status;
		}
		status = writeFloatIrregularTimeSeries(ifltab, path, date, time, 0); // write floats without gaps
	}
	else { // write floats first
		path = path_float;
		status = writeFloatIrregularTimeSeries(ifltab, path, date, time, 1); // write floats with gaps
		if (status != STATUS_OKAY) {
			return status;
		}
		status = writeDoubleIrregularTimeSeries(ifltab, path, date, time, 0); // write doubles without gaps

	}

	status = check_values_compare(ifltab, path, 1);

	return status;
}



/// <summary>
/// Writes doubles to a time-series with missing data
/// Writes floats to a time-series filling in the missing data.
/// </summary>
/// <returns></returns>
int write_ts_mixed(long long* ifltab, int writeDoublesFirst) {
	int status = 1;
	const char* date = "30Jun2024";
	const char* time = "2300";

	char* path_float = "//GAPT_DAM/FLOW-LOCAL//1Hour/GAPT_HMS_FORECAST_floats_first/";
	char* path_double = "//GAPT_DAM/FLOW-LOCAL//1Hour/GAPT_HMS_FORECAST_doubles_first/";
	char* path;
	if (writeDoublesFirst) {
		path = path_double;
		status = writeDoublesTimeSeries(ifltab, path, date, time, 1); // write doubles with gaps
		if (status != STATUS_OKAY) {
			return status;
		}
		status = writeSingleTimeSeries(ifltab, path, date, time, 0); // write floats without gaps
	}
	else { // write floats first
		path = path_float;
		status = writeSingleTimeSeries(ifltab, path, date, time, 1); // write floats with gaps
		if (status != STATUS_OKAY) {
			return status;
		}
		status = writeDoublesTimeSeries(ifltab, path, date, time, 0); // write doubles without gaps

	}

	status = check_values_compare(ifltab, path, 0);

	return status;
}

int test_mixed_record_types() {

	long long ifltab[250];
	const char* dssFileName = "test_create_mixed_record_types.dss";
	deleteFile(dssFileName);

	int status = hec_dss_zopen(ifltab, dssFileName);

	if (status != STATUS_OKAY) {
		return status;
	}
	// -- Irregular Interval --

	status = write_irregular_ts_mixed(ifltab, 0);
	if (status != STATUS_OKAY) {
		return status;
	}


	status = write_irregular_ts_mixed(ifltab, 1);
	if (status != STATUS_OKAY) {
		return status;
	}


	// -- Regular Interval --

	status = write_ts_mixed(ifltab, 0);
	if (status != STATUS_OKAY) {
		return status;
	}
	status = write_ts_mixed(ifltab, 1);
	if (status != STATUS_OKAY) {
		return status;
	}
	// check that all records are doubles.

	status = check_catalog_for_doubles(ifltab);


	zclose(ifltab);

	return status;

}

int writeDoublesTimeSeries(long long* ifltab, const char* path, const char* date, const char* time, int some_missing_data) {
	double dvalues[NUM_TS_VALUES];
	for (int i = 0; i < NUM_TS_VALUES; i++) {
		if (i % 3 == 0 && some_missing_data) {
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
		if (i % 3 == 0 && some_missing_data) {
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

int writeDoubleIrregularTimeSeries(long long* ifltab, const char* path, const char* date, const char* time, int some_missing_data) {
	double dvalues[NUM_TS_VALUES];
	int itimes[NUM_TS_VALUES];
	char* cnull = 0;

	int julian = dateToJulian(date);
	int seconds = timeStringToSeconds(time);
	int mins = julian * 1440 + seconds / 60;
	for (int i = 0; i < NUM_TS_VALUES; i++) {
		if (i % 6 == 0 && some_missing_data) {
			dvalues[i] = UNDEFINED_DOUBLE;
		}
		else {
			dvalues[i] = (double)i;
		}
		itimes[i] = mins + (i * 60);
	}

	zStructTimeSeries* tss = zstructTsNewIrregDoubles(path, dvalues, NUM_TS_VALUES, itimes, MINUTE_GRANULARITY, cnull, "cfs", "Inst-Val");
	int status = ztsStore(ifltab, tss, 0);
	zstructFree(tss);
	return status;
}
int writeFloatIrregularTimeSeries(long long* ifltab, const char* path, const char* date, const char* time, int some_missing_data) {
	float fvalues[NUM_TS_VALUES];
	int itimes[NUM_TS_VALUES];
	char* cnull = 0;

	int julian = dateToJulian(date);
	int seconds = timeStringToSeconds(time);
	int mins = julian * 1440 + seconds / 60;
	for (int i = 0; i < NUM_TS_VALUES; i++) {
		if (i % 6 == 0 && some_missing_data) {
			fvalues[i] = UNDEFINED_FLOAT;
		}
		else {
			fvalues[i] = (float)i;
		}
		itimes[i] = mins + (i * 60);
	}

	zStructTimeSeries* tss = zstructTsNewIrregFloats(path, fvalues, NUM_TS_VALUES, itimes, MINUTE_GRANULARITY, cnull, "cfs", "Inst-Val");
	int status = ztsStore(ifltab, tss, 0);
	zstructFree(tss);
	return status;
}