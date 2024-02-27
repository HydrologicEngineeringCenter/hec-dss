#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#if defined(__linux__) || defined(__APPLE__) || defined(__sparc)
#include <unistd.h>
#else
#define strtok_r strtok_s
#define strdup _strdup
#include <direct.h>
#endif
#include "heclib.h"
#include "hecdss7.h"
#include "TestDssC.h"



int runTheTests();


int gridMemoryTest() {

	long long ifltab[250] = { 0 };
	int status = hec_dss_zopen(ifltab, "2017-06-28_event.dss");
	if (status != 0) {
		printf("Error during open.  status= %d\n", status);
		return status;
	}
	char* path = "/SHG1k/Iowa50km/PRECIPITATION/26JUN2017:2300/26JUN2017:2400/AORC/";
	if (status < 0) {
		printf("Error during catalog.  Error code %d\n", status);
		return status;
	}
	zStructSpatialGrid* gridStructRetrieve;

	for (int i = 0; i < 100; i++)
	{
		gridStructRetrieve = zstructSpatialGridNew(path);
		status = zspatialGridRetrieve(ifltab, gridStructRetrieve, 1);

		zstructFree(gridStructRetrieve);
		//zspatialGridStructFree(gridStructRetrieve);
		if (status != STATUS_OKAY) {
			printf("Error retrieving grid: %d", status);
			return status;
		}
	}
	zclose(ifltab);
	return 0;
}


void usage(char* exeName)
{
	printf("Version: %s %s\n", __DATE__, __TIME__);

	printf("\nUsage:\n %s command [options] [dssfilename]  ", exeName);
	printf("\ncommands: test|catalog|zqueeze|lock seconds|check-lock|zcheck(File|Links|Pathnames) |workout version timeSeriesCount timeSeriesLength");
	printf("\nWhere:");
	printf("\ntest, runs standard set of DSS tests (needs to run in directory with test files.");
	printf("\ncatalog, prints the DSS catalog to the console");
	printf("\nconvert input.dss  converted.dss, converts from dss6 to dss7 (or dss7 to dss6)");
	printf("\nzqueeze, rebuilds the DSS file, recovering space");
	printf("\nzcheckFile , runs agressive test of DSS file");
	printf("\nlock seconds, locks file for seconds seconds");
	printf("\ncheck-lock, displays lock status to console");
	printf("\nzcheckLinks , medium level DSS test");
	printf("\nzcheckPathnames , quick test of DSS file");
	printf("\ncheck-lock , check locking");
	printf("\nfile.dss DSS file");
	printf("\nworkout, performs reads/writes on file.dss, can be used with multiple instances");
	printf("\nversion, dss version (6 or 7)");
	printf("\nexport file.dss path metaDataOnly(0|1)  # writes the contents of a DSS record to the console");
	printf("\nimport-profile input.csv output.dss path date time units datatype");

	printf("\n%s pathnameTesting 6|7,  tests large fparts such as when using collections", exeName);


	printf("\n\nExamples:\n%s workout 7 2000 5000 test.dss", exeName);
	printf("\n%s test", exeName);
	printf("\n%s catalog myfile.dss", exeName);
	printf("\n%s convert input.dss  converted.dss", exeName);
	printf("\n%s catalog myfile.dss details     # includes record type in output", exeName);
	printf("\n%s zsqueeze myfile.dss", exeName);
	printf("\n%s lock 15 myfile.dss", exeName);
	printf("\n%s check-lock myfile.dss", exeName);
	printf("\n%s zcheckFile myfile.dss", exeName);
	printf("\n%s zcheckLinks myfile.dss", exeName);;
	printf("\n%s zcheckPathnames myfile.dss", exeName);
	printf("\n%s export myfile.dss /SHG/EFRUSSIAN20/PRECIPITATION/01OCT2004:2400/02OCT2004:0100/GAGEINTERP/ 1", exeName);
	printf("\n%s pathnameTesting file.dss 7", exeName);
	printf("\n%s import-profile input.csv output.dss //location1/id-depth//10Second/ADCIRC-run12/ 2022-05-04 10:12:30 feet INST-VAL", exeName);
	printf("\n");

	printf("\nSupported Environmnet variable examples:");
	printf("\nDSS_DEBUG_LEVEL=17");

}

int main(int argc, char* argv[])
{

	int status = 0;
	long long start_time = getCurrentTimeMillis();

	char* debugLevel = getenv("DSS_DEBUG_LEVEL");
	if (debugLevel && strlen(debugLevel) > 0) {
		int mlev = atoi(debugLevel);
		zset("mlev", "", mlev);
	}

	if (argc < 2)
	{
		usage(argv[0]);
		return -1;
	}
	else if (argc == 2 && strcmp(argv[1], "test") == 0) { // test
		status = runTheTests();

	}

	else if ((argc == 3 || argc == 4) && strcmp(argv[1], "catalog") == 0)
	{ // ./exe catalog myfile.dss
		int printRecordType = 0;
		if (argc == 4 && strcmp(argv[3], "details") == 0)
			printRecordType = 1;
		status = PrintCatalog(argv[2], printRecordType);
	}
	else if (argc == 3 && strcmp(argv[1], "zsqueeze") == 0)
	{ // ./exe zsqueeze myfile.dss
		status = Zqueeze(argv[2]);
	}
	else if (argc == 3 && strcmp(argv[1], "zcheckFile") == 0)
	{ // ./exe zcheckFile myfile.dss
		status = CheckFile(argv[2]);
	}
	else if (argc == 3 && strcmp(argv[1], "zcheckLinks") == 0)
	{ // ./exe zcheckLinks myfile.dss
		status = CheckLinks(argv[2]);
	}
	else if (argc == 3 && strcmp(argv[1], "zcheckPathnames") == 0)
	{ // ./exe zcheckLinks myfile.dss
		status = CheckPathnames(argv[2]);
	}

	else if (argc == 6 && strcmp(argv[1], "workout") == 0)
	{// example:  workout 7 test.dss
		status = Workout(argv[0], argv[2], argv[3], argv[4], argv[5]);
	}
	else if (argc == 3 && strcmp(argv[1], "check-lock") == 0)
	{// example:  check-lock test.dss
		status = CheckLocking(argv[2]);

	}
	else if (argc == 4 && strcmp(argv[1], "lock") == 0)
	{// example:  lock 12 test.dss
		status = Lock(argv[3], atoi(argv[2]));
	}
	else if (strcmp(argv[1], "export") == 0 && (argc == 4 || argc == 5)) { // export file.dss path metaDataOnly(0|1)
		int metaDataOnly = 0;
		if (argc == 5) metaDataOnly = strcmp(argv[4], "1") ? 0 : 1;
		status = Export(argv[2], argv[3], metaDataOnly);
	}
	else if (argc == 3 && strcmp(argv[1], "recordinfo") == 0) {
		testRecordInfo6(argv[2]);
	}
	else if (argc == 4 && strcmp(argv[1], "pathnameTesting") == 0) {
		status = PathnameTesting(argv[2], atoi(argv[3]));
	}
	else if (argc == 9 && strcmp(argv[1], "import-profile") == 0) {
		//int ImportProfile(const char* csvFilename, const char* dssFilename, const char* path, const char* date, const char* time, const char* units, const char* datatype);
		status = ImportProfile(argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8]);
	}
	else if (argc == 4 && strcmp(argv[1],"convert")==0 )
	{
		status = zconvertVersion(argv[2], argv[3]);
	}
	else
	{
		usage(argv[0]);
	}

	printf("\nStatus = %d", status);
	double elapsed = (getCurrentTimeMillis() - start_time) / 1000.0;

	printf("\nSeconds elapsed: %f", elapsed);

	return status;
}
int runTheTests() {
	long long ifltab7[250] = { 0 };
	long long ifltab6[250] = { 0 };
	char fileName7[80];
	char fileName7a[80];
	char fileName6[80];
	int status;

	printf("test read without dates\n");
	status = testNoDates(7);
	if (status != STATUS_OKAY)
		return status;
	status = testNoDates(6);
	if (status != STATUS_OKAY)
		return status;


	printf("test issue CWMS-1424 (Time Series Store Rule)\n");
	status = testTsStoreRules();
	if (status != STATUS_OKAY)
		return status;

	printf("test issue DSS-178\n");
	if (bigEndian()) zset("disa", "", -1);
	status = testDss178();
	zset("disa", "", 0);
	if (status != STATUS_OKAY)
		return status;

	printf("test text tables issue 135\n");
	status = testTextTableIssue135();
	if (status != STATUS_OKAY)
		return status;

	printf("\ntest odd number values\n");
	status = testOddNumberValues();
	if (status != STATUS_OKAY) {
		zset("disa", "", -1);
		status = testOddNumberValues();
		zset("disa", "", 0);
		if (status != STATUS_OKAY) {
			return status;
		}
	}

	printf("\ntest pseudo-regular 8Minute data\n");
	status = testPseudoEightHourIrregular();
	if (status != STATUS_OKAY) {
		zset("disa", "", -1);
		status = testPseudoEightHourIrregular();
		zset("disa", "", 0);
		if (status != STATUS_OKAY) {
			return status;
		}
	}

	printf("\ntest zinquire return value for FVER\n");
	status = fver_test();
	if (status != STATUS_OKAY)
		return status;

	printf("\ntest Jira DSS-163 weekly time series issue\n");
	status = test_jira_dss_163_weekly_time_series_fails();
	if (status != STATUS_OKAY)
		return status;

	printf("\ntest copy large record\n");
	status = testLargeCopy();
	if (status != STATUS_OKAY)
		return status;

	printf("\ntest miscellaneous stuff\n");
	status = miscTests();
	if (status != STATUS_OKAY)
		return status;


	printf("\ntest grid memory\n");
	status = gridMemoryTest();
	if (status != STATUS_OKAY)
		return status;

	printf("\ntest units issue 126\n");
	status = units_issue_126();
	if (status != STATUS_OKAY)
		return status;


	printf("\ntest format F part with tags\n");
	status = test_normalize_f_part();
	if (status != STATUS_OKAY)
		return status;

	printf("\ntest vertical datum operations (C API)\n");
	status = test_vertical_datums_c();
	if (status != STATUS_OKAY)
		return status;

	status = PathnameTesting("path_name_test7.dss",7);
	status = PathnameTesting("path_name_test6.dss", 6);

	printf("\ntest stringCat\n");
	status = test_stringCat();
	if (status != STATUS_OKAY)
		return status;

	printf("\ntest Bulletin_17C_Examples.dss for reading full record\n");
	status = Bulletin_17C_SSP_Issue();
	if (status != STATUS_OKAY)
		return status;



	printf("\ntestDateFunctions\n");
	status = testDateFunctions();
	if (status != STATUS_OKAY) return status;

	remove("testmultiuser7.dss");
	status = testMultiUser("testmultiuser7.dss", 7, 333, 2);
	if (status != STATUS_OKAY) return status;

	remove("testmultiuser6.dss");
	status = testMultiUser("testmultiuser6.dss", 6, 444, 2);
	if (status != STATUS_OKAY) return status;




	//findInFile();
	//mainx(argc, argv);
	//return 0;

	//catStruct = zstructCatalogNew();
	//testMultiUser();
	//return 0;

	//catStruct = zstructCatalogNew(); 

	//status = hec_dss_zopen(ifltab7, "C:\\Users\\q0hecwjc\\Desktop\\funny.dss");
	//status = hec_dss_zopen(ifltab7, "C:\\Users\\q0hecwjc\\Desktop\\db7.dss");
	//status = zcatalog((long long*)ifltab7, catStruct->pathWithWildChars, catStruct, 0);

	//testCatalog();
	//testConversion();
	//zset("MLVL", "", 15);
	//stringCopy(fileName7, sizeof(fileName7), "C:/Users/q0hecwjc/Desktop/snow.2016.01.dss", sizeof(fileName7));	
	//stringCopy(fileName7, sizeof(fileName7), "C:/Users/q0hecwjc/Desktop/airtemp.2016.04.dss", sizeof(fileName7));	
	//stringCopy(fileName7, sizeof(fileName7), "C:/Users/q0hecwjc/Documents/test.dss", sizeof(fileName7));
	//status = hec_dss_zopen(ifltab7, fileName7);
	//if (status) return status;
	//status =spatialDateTime ("01DEC2016:2400", &jul, &jul2);
	//status = testAdHoc2();
//	filesToUnix();
//	return;
	//testConversion();
	//status = testAdHoc2();
	//bc1();
	//ExampleOpenx();
	//return;
//	if (status != STATUS_OKAY) return status;

	status = testMisc();
	if (status != STATUS_OKAY) return status;

	status = testDateTime();
	if (status != STATUS_OKAY) return status;

	status = testDelete("testUtilities7.dss", "testUtilities6.dss");
	if (status != STATUS_OKAY) return status;

	stringCopy(fileName7, sizeof(fileName7), "testDss7.dss", sizeof(fileName7));
	remove(fileName7);

	status = hec_dss_zopen(ifltab7, fileName7);
	if (status) return status;

	//status = testztsStruct1(ifltab7);
	//zclose(ifltab7);
	//printf("Done\n");
	//if (status != STATUS_OKAY) return status;
	//return status;

	printf("\n\n\n\n###################################################################\n\n");
	printf("     Test 1 - Normal\n");
	printf("\n###################################################################\n\n");

	status = runTests(ifltab7);
	if (status != STATUS_OKAY) return status;
	zclose(ifltab7);


	status = testCatalog();
	if (status != STATUS_OKAY) return status;


	status = testConversion();
	if (status != STATUS_OKAY) return status;

	//printf("\n\n\n#####################  Completion\n\n\n");
	//if (status == STATUS_OKAY) return status;

	//  Test reclamation
	stringCopy(fileName7a, sizeof(fileName7a), "testReclaim7.dss", sizeof(fileName7));
	remove(fileName7a);
	status = testReclaim(fileName7a);
	if (status != STATUS_OKAY) return status;

	stringCopy(fileName6, sizeof(fileName6), "testDss6.dss", sizeof(fileName6));
	remove(fileName6);
	status = zopen6(ifltab6, fileName6);
	if (status != STATUS_OKAY) return status;
	status = hec_dss_zopen(ifltab7, fileName7);
	if (status) return status;
	status = testIO_Interface(ifltab7, ifltab6);
	if (status != STATUS_OKAY) return status;

	printf("\n\n\n\n###################################################################\n\n");
	printf("     Test 2 - DSS-6\n");
	printf("\n###################################################################\n\n");
	status = runTests(ifltab6);
	if (status != STATUS_OKAY) return status;
	printf("\n\n\n#####################  Completion\n\n\n");

	status = zcatalogFile(ifltab7, "dss7.txt", 0, (const char*)0);
	if (status < 1) {
		printf("\n\nCatalog FAILED, status = %d\n", status);
		return status;
	}

	zclose(ifltab7);

	status = testSqueeze(fileName7);
	if (status != STATUS_OKAY) return status;

	printf("\n\nNormal tests passed....\n");
	//return 0;

	printf("\n\nNow detune (should run really slow!)\n\n\n");

	//  Try the tests with the file detuned
	remove(fileName7);
	status = hec_dss_zopen(ifltab7, fileName7);
	if (status != STATUS_OKAY) return status;
	zsetFile(ifltab7, "detune", "", 1);

	printf("\n\n\n\n###################################################################\n\n");
	printf("     Test 3 - Detuned file\n");
	printf("\n###################################################################\n\n");

	status = runTests(ifltab7);
	if (status != STATUS_OKAY) return status;
	printf("\n\n\n#####################  Completion\n\n\n");

	//  Try the tests with the space reclamation off
	printf("\n\nDetuned tests passed....\nNow turn off space reclamation\n\n\n");
	remove(fileName7);
	status = hec_dss_zopen(ifltab7, fileName7);
	if (status != STATUS_OKAY) return status;
	zsetFile(ifltab7, "reclaim", "", 0);

	printf("\n\n\n\n###################################################################\n\n");
	printf("     Test 4 - No space reclamation\n");
	printf("\n###################################################################\n\n");

	status = runTests(ifltab7);
	if (status != STATUS_OKAY) return status;
	zclose(ifltab7);
	printf("\n\n\n#####################  Completion\n\n\n");

	//  Try the tests with the file detuned and space reclamation off
	printf("\n\nTests passed....\nNow detune and turn off space reclamation\n\n\n");
	remove(fileName7);
	status = hec_dss_zopen(ifltab7, fileName7);
	if (status != STATUS_OKAY) return status;
	zsetFile(ifltab7, "detune", "", 1);
	zsetFile(ifltab7, "reclaim", "", 0);

	printf("\n\n\n\n###################################################################\n\n");
	printf("     Test 5 - Detuned file and No space reclamation\n");
	printf("\n###################################################################\n\n");

	status = runTests(ifltab7);
	if (status != STATUS_OKAY) return status;
	zclose(ifltab7);
	printf("\n\n\n#####################  Completion\n\n\n");
	//	zsetMessageLevel(zmessaging_global_id, MESS_INTERNAL_DIAG_1);

	for (int i = 0; i < maxMessageAvail; i++) {
		// For intel inspector.
		free(zmessageAvail.messages[i]);
		zmessageAvail.messages[i] = NULL;
	}


	printf("\n\nAll tests passed!\n\nGoodbye DSS-7\n");
	return 0;
}



int writeDoubleArray() {
	long long ifltab[250];

	zStructArray* struct1;
	int nvalues = 1;
	//char path1[MAX_PATHNAME_LENGTH];
	double doublevalues[1];
	doublevalues[0] = 0.1;
	char* path = "//wat-test/Total/Simulation//wat-issue/";
	//stringCopy(path1, sizeof(path1), "//wat-test/Total/Simulation//wat-issue/", 50);

	struct1 = zstructArrayNew(path);
	struct1->doubleArray = doublevalues;
	struct1->numberDoubleArray = nvalues;

	deleteFile("C:/temp/testarray.dss");
	int status = hec_dss_zopen(ifltab, "C:/temp/testarray.dss");

	status = zarrayStore(ifltab, struct1);
	if (status != STATUS_OKAY) {
		printf("Write on testArrayWriteRead doubles Failed!\n");
		return status;
	}
	return status;
}


int multipleWriteDeleteSlowDown() {

	long long ifltab[250], mills1, mills2, diff;
	zStructPairedData* pds1;
	float fordinates[500], fvalues[500];
	int status, i;

	deleteFile("C:/temp/testpd.dss");
	status = hec_dss_zopen(ifltab, "C:/temp/testpd.dss");
	//zsetfi_(ifltab,"reclaim", " ", RECLAIM_NONE,&k,&status,7,1);

	//zsetfi_(ifltab, "FMULT", "ON", &number, &status,
		//5, 2);

	if (status != STATUS_OKAY) return status;

	for (i = 0; i < 500; i++) {
		fordinates[i] = (float)i;
		fvalues[i] = (float)i;
	}

	pds1 = zstructPdNewFloats("/test/paired-data-container/x-y///slowdown-test/", fordinates, fvalues, 500, 1,
		"Feet", "Unt", "cfs", "Unt");

	for (i = 0; i < 10001; i++) {
		mills1 = getCurrentTimeMillis();
		status = zpdStore(ifltab, pds1, 0);
		if (status != STATUS_OKAY) {
			printf("Failed on write, loop %d\n", i);
			return status;
		}
		status = zdelete(ifltab, pds1->pathname);
		mills2 = getCurrentTimeMillis();
		diff = mills2 - mills1;
		printf("Loop %d, time = %lld\n", i, diff);
		if (status != STATUS_OKAY) {
			printf("Failed on delete, loop %d\n", i);
			return status;
		}
	}

	zstructFree(pds1);
	zclose(ifltab);
	return status;
}




int SolarisTesting()
{
	long long ifltab[250];
	zStructTimeSeries* tss1, * tss2;
	float fvalues[200];

	for (int i = 0; i < 200; i++) {
		fvalues[i] = (float)(i + 1);
	}

	int status = hec_dss_zopen(ifltab, "charlong_7.dss");
	if (status) return status;


	//status = zopen6(ifltab6, fileName6);
	//if (status != STATUS_OKAY) return status;

	tss1 = zstructTsNewRegFloats("/Basin/Location/Flow//30Min/TSS-Floats/", fvalues, 200, "21Jan2001", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct1 Loc 1, store status ")) return status;


	tss2 = zstructTsNew("/Basin/Location/Flow/01Jan2001/30Min/TSS-Floats/");
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct1 Loc 2, retrieve status ")) return status;

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testztsStruct1, Location 3");
	return status;


}

int UnitPaddingIssue()
{
	long long ifltab[250];
	char* path = "//Dry Creek/Flow/01Feb2014/1Hour/N0H0B0";
	zStructTimeSeries* tss1 = zstructTsNew(path);
	int status = hec_dss_zopen(ifltab, "k7-small.dss");
	if (status) return status;

	status = ztsRetrieve(ifltab, tss1, -1, 1, 0);
	printf("\nunits= '%s'", tss1->units);
	printf("\ntype= '%s'", tss1->type);
	printf("\ntimeZoneName= '%s'", tss1->timeZoneName);
	printf("\n");

	long long ifltab2[250];
	deleteFile("k7-small-updated.dss");
	status = hec_dss_zopen(ifltab2, "k7-small-updated.dss");
	tss1->pathname = "//Dry Creek/Flow/01Feb2014/1Hour/karl";
	ztsStore(ifltab2, tss1, 0);
	zstructFree(tss1);
	zStructTimeSeries* tss2 = zstructTsNew(path);


	zcopyFile(ifltab, ifltab2, 0);
	status = ztsRetrieve(ifltab2, tss2, -1, 1, 0);
	printf("\nunits= '%s'", tss2->units);
	printf("\ntype= '%s'", tss2->type);
	printf("\ntimeZoneName= '%s'", tss2->timeZoneName);
	printf("\n");
	zstructFree(tss2);
	return status;
}

void decodeError(int errorCode)
{
	int highFunction;
	int lowFunction;
	int dssError;
	int status;
	int severity = zerrorDecode(errorCode, &highFunction, &lowFunction, &dssError, &status);
	char message[300];
	int size = 299;
	zerrorMessage(message, size, severity, dssError, lowFunction);
	printf("\n%s\n", message);
	zerrorMessage(message, size, severity, dssError, highFunction);
	printf("\n%s\n", message);

}

int units_issue_126()
{
	const char* dssFileName = "units_issue_126.dss";

	deleteFile(dssFileName);

	char* path = "//Subbasin-3/TEMPERATURE-MINIMUM//1DAY/MET:GageWts Daily/";
	zStructTimeSeries* tss1 = zstructTsNewTimes(path, "15 January 1974", "24:00", "25 January 1974", "24:00");

	float* fvalues = malloc(11 * sizeof(float));
	if (fvalues == 0)
		return -1;
	fvalues[0] = 1;
	fvalues[1] = 2;
	fvalues[2] = 3;
	fvalues[3] = 4;
	fvalues[4] = 5;
	fvalues[5] = 6;
	fvalues[6] = 7;
	fvalues[7] = 8;
	fvalues[8] = 9;
	fvalues[9] = 10;
	fvalues[10] = 11;
	tss1->numberValues = 11;
	tss1->floatValues = fvalues;
	tss1->units = "DEG C";
	tss1->type = "INST-VAL";

	long long ifltab[250];
	memset(ifltab, 0, sizeof(ifltab));
	int status = zopen6(ifltab, dssFileName);
	if (status) return status;

	ztsStore(ifltab, tss1, 0);
	zstructFree(tss1);
	zclose(ifltab);

	zStructTimeSeries* tss2 = zstructTsNew(path);
	long long ifltab2[250];
	memset(ifltab2, 0, sizeof(ifltab2));
	hec_dss_zopen(ifltab2, dssFileName);
	status = ztsRetrieve(ifltab2, tss2, -1, 1, 0);

	printf("\nunits= '%s'", tss2->units);
	printf("\ntype= '%s'", tss2->type);
	printf("\ntimeZoneName= '%s'", tss2->timeZoneName);

	printf("\n");

	const char* expectedUnits = "DEG C";

	if (strncmp(expectedUnits, tss2->units, strlen(expectedUnits))) {
		status = -2;
	}
	if (strncmp("INST-VAL", tss2->type, 8) != 0)
		status = -3;

	zstructFree(tss2);
	zclose(ifltab2);
	free(fvalues);
	return status;
}

int test_jira_dss_163_weekly_time_series_fails()
{
	const char* filename = "jira_dss-163.dss";
	const char* pathnameTemplate = "//TestLoc/Elev//1Week/%4.4d-%2.2d-%2.2d/";
	char pathname[MAX_PATHNAME_SIZE];
	char fpart[MAX_PART_SIZE];
	const char* unit = "ft";
	const char* dataType = "INST-VAL";
	char startdate[16];
	char enddate[16];
	char datestr[16];
	char timestr[8];
	char datestr2[16];
	char timestr2[8];
	const char* starttime = "0100";
	const char* endtime = "0100";
	const int NUM_VALUES = 26;
	zStructTimeSeries* tss1;
	zStructTimeSeries* tss2;
	ztsTimeWindow* tw;
	int msgLevel;
	double dvalues[52];
	for (int i = 0; i < NUM_VALUES; ++i) {
		dvalues[i] = 1000. + i;
	}
	long long ifltab[250];
	memset(ifltab, 0, sizeof(ifltab));
	memset(fpart, 0, sizeof(fpart));

	unlink(filename);
	int status = hec_dss_zopen(ifltab, filename);


	for (int year = 1900; year <= 2100; year += 10) {
		for (int month = 1; month < 2; ++month) {
			for (int day = 1; day < 8; ++day) {
				sprintf(pathname, pathnameTemplate, year, month, day);
				int jul = yearMonthDayToJulian(year, month, day);
				julianToDate(jul, -13, startdate, sizeof(startdate));
				julianToDate(jul + (NUM_VALUES - 1) * 7, -13, enddate, sizeof(enddate));
				//----------------------//
				// store by time window //
				//----------------------//
				tss1 = zstructTsNewTimes(pathname, startdate, starttime, enddate, endtime);
				tss1->numberValues = NUM_VALUES;
				tss1->doubleValues = dvalues;
				tss1->units = (char*)unit;
				tss1->type = (char*)dataType;
				status = ztsStore(ifltab, tss1, 0);
				zstructFree(tss1);
				if (status != STATUS_OKAY) {
					printf("Status = %d\n", status);
					zclose(ifltab);
					return status;
				}
				//-------------------------//
				// retrieve by time window //
				//-------------------------//
				tss2 = zstructTsNewTimes(pathname, startdate, starttime, enddate, endtime);
				status = ztsRetrieve(ifltab, tss2, -1, 2, 1);
				if (status != STATUS_OKAY) {
					printf("Status = %d\n", status);
					zstructFree(tss2);
					zclose(ifltab);
					return status;
				}
				if (status == 0) {
					int jul = tss2->startJulianDate;
					int sec = tss2->startTimeSeconds;
					status = tss2->numberValues == NUM_VALUES ? STATUS_OKAY : STATUS_NOT_OKAY;
					for (int i = 0; i < tss2->numberValues; ++i) {
						getDateTimeString(jul, datestr, sizeof(datestr), -13, sec, timestr, sizeof(timestr), 0);
						//printf("\t%s %s %.2f\n", datestr, timestr, tss2->doubleValues[k]);
						incrementTime(tss2->timeIntervalSeconds, 1, jul, sec, &jul, &sec);
						if (i == 0) {
							status |= strcmp(datestr, startdate) || strcmp(timestr, starttime);
						}
						status |= tss2->doubleValues[i] != 1000. + i;
						if (status != STATUS_OKAY) {
							zstructFree(tss2);
							zclose(ifltab);
							return status;
						}
					}
				}
				zstructFree(tss2);
			}
		}
	}
	//------------------//
	// catalog the file //
	//------------------//
	zStructCatalog* cs = zstructCatalogNew();
	zcatalog(ifltab, "/*/*/*/*/*/*/", cs, 1);
	//----------------------//
	// retrieve by pathname //
	//----------------------//
	for (int i = 0; i < cs->numberPathnames; ++i) {
		//printf("%d\t%s\n", k, cs->pathnameList[k]);
		zpathnameGetPart(cs->pathnameList[i], 6, fpart, sizeof(fpart));
		//---------------------------//
		// retrieve by pathname only //
		//---------------------------//
		tss2 = zstructTsNew(cs->pathnameList[i]);
		status = ztsRetrieve(ifltab, tss2, -1, 2, 1);
		if (status != STATUS_OKAY) {
			printf("\terror %d retrieving %s\n", status, cs->pathnameList[i]);
			zstructFree(tss2);
			zclose(ifltab);
			return status;
		}
		if (status == 0) {
			int jul = tss2->startJulianDate;
			int sec = tss2->startTimeSeconds;
			for (int j = 0; j < tss2->numberValues; ++j) {
				getDateTimeString(jul, datestr, sizeof(datestr), 4, sec, timestr, sizeof(timestr), 0);
				getDateTimeString(jul, datestr2, sizeof(datestr2), -13, sec, timestr2, sizeof(timestr), 0);
				//printf("\t%s %s %.2f\n", datestr, timestr, tss2->doubleValues[k]);
				incrementTime(tss2->timeIntervalSeconds, 1, jul, sec, &jul, &sec);
				if (j == 0) {
					status |= strcmp(datestr2, fpart) || strcmp(timestr2, starttime);
					if (status) {
						printf(
							"%d\t%s\t%d (%d)\t%s\t%d (%d)\t%d (%d)\n",
							i,
							cs->pathnameList[i],
							dateToJulian(fpart),
							dayOfWeek(dateToJulian(fpart)),
							datestr,
							dateToJulian(datestr),
							dayOfWeek(dateToJulian(datestr)),
							tss2->startJulianDate,
							dayOfWeek(tss2->startJulianDate));
						zstructFree(tss2);
						zclose(ifltab);
						return status;
					}
				}
				minsToDateTime(tss2->julianBaseDate * MINS_IN_1_DAY + tss2->times[j], datestr2, timestr2, sizeof(datestr2), sizeof(timestr2));
				status |= strcmp(datestr2, datestr) || strcmp(timestr2, timestr);
				if (status) {
					printf("\tExpected %s %s, got %s %s", datestr, timestr, datestr2, timestr2);
					zstructFree(tss2);
					zclose(ifltab);
					return status;
				}
				status |= tss2->doubleValues[j] != 1000. + j;
				if (status) {
					printf("\tExpected %.2f, got %.2f", 1000. + j, tss2->doubleValues[j]);
					zstructFree(tss2);
					zclose(ifltab);
					return status;
				}
			}
		}
		zstructFree(tss2);
	}
	//printf("\n");
	for (int i = 0; i < cs->numberPathnames; ++i) {
		zpathnameGetPart(cs->pathnameList[i], 6, fpart, sizeof(fpart));
		//--------------------------------------//
		// retrieve by pathname and block times //
		//--------------------------------------//
		tw = (ztsTimeWindow*)calloc(1, sizeof(ztsTimeWindow));
		if (!tw) {
			printf("\nMEMORY ERROR\n");
			zstructFree(tss2);
			zclose(ifltab);
			return -1;
		}
		ztsGetPathTimeWindow(7, cs->pathnameList[i], strlen(cs->pathnameList[i]), tw);
		int startJul = tw->startBlockJulian;
		int endJul = ztsIncrementBlock(startJul, tw->blockSize);
		--endJul;
		free(tw);
		julianToDate(startJul, -13, startdate, sizeof(startdate));
		julianToDate(endJul, -13, enddate, sizeof(enddate));
		tss2 = zstructTsNewTimes(cs->pathnameList[i], startdate, "0001", enddate, "2400");
		status = ztsRetrieve(ifltab, tss2, -1, 2, 1);
		if (status != STATUS_OKAY) {
			printf("\terror %d retrieving %s\n", status, cs->pathnameList[i]);
			zstructFree(tss2);
			zclose(ifltab);
			return status;
		}
		if (status == 0) {
			int jul = tss2->startJulianDate;
			int sec = tss2->startTimeSeconds;
			for (int j = 0; j < tss2->numberValues; ++j) {
				getDateTimeString(jul, datestr, sizeof(datestr), 4, sec, timestr, sizeof(timestr), 0);
				getDateTimeString(jul, datestr2, sizeof(datestr2), -13, sec, timestr2, sizeof(timestr), 0);
				//printf("\t%s %s %.2f\n", datestr, timestr, tss2->doubleValues[k]);
				incrementTime(tss2->timeIntervalSeconds, 1, jul, sec, &jul, &sec);
				if (j == 0) {
					status |= strcmp(datestr2, fpart) || strcmp(timestr2, starttime);
					if (status) {
						printf(
							"%d\t%s\t%d (%d)\t%s\t%d (%d)\t%d (%d)\n",
							i,
							cs->pathnameList[i],
							dateToJulian(fpart),
							dayOfWeek(dateToJulian(fpart)),
							datestr,
							dateToJulian(datestr),
							dayOfWeek(dateToJulian(datestr)),
							tss2->startJulianDate,
							dayOfWeek(tss2->startJulianDate));
						zstructFree(tss2);
						zclose(ifltab);
						return status;
					}
				}
				minsToDateTime(tss2->julianBaseDate * MINS_IN_1_DAY + tss2->times[j], datestr2, timestr2, sizeof(datestr2), sizeof(timestr2));
				status |= strcmp(datestr2, datestr) || strcmp(timestr2, timestr);
				if (status) {
					printf("\tExpected %s %s, got %s %s", datestr, timestr, datestr2, timestr2);
					zstructFree(tss2);
					zclose(ifltab);
					return status;
				}
				status |= tss2->doubleValues[j] != 1000. + j;
				if (status) {
					printf("\tExpected %.2f, got %.2f", 1000. + j, tss2->doubleValues[j]);
					zstructFree(tss2);
					zclose(ifltab);
					return status;
				}
			}
		}
		zstructFree(tss2);
	}
	zstructFree(cs);
	zclose(ifltab);
	return status;
}

int fver_test() {
	const char* filename = "fvers-test.dss";
	long long ifltab[250] = { 0 };
	char alpha[80];
	int  number;
	int  status;
	int  major, minor, patch;
	int  expected;

	zquery("VERS", alpha, sizeof(alpha), &number);
	major = alpha[0] - '0';
	minor = alpha[2] - '@';
	patch = alpha[3] - '@';
	expected = major * 10000 + minor * 100 + patch;

	remove(filename);
	status = hec_dss_zopen(ifltab, filename);
	if (status != 0) {
		printf("Could not open DSS file %s\n", filename);
	}
	else {
		zinquireChar(ifltab, "FVER", alpha, sizeof(alpha), &number);
		if (number != expected) {
			printf("Expected numeric output from zinquireChar(ifltab, \"FVER\", alpha, number) to equal %d, but got %d\n", expected, number);
			status = -1;
		}

		number = zinquire(ifltab, "FVER");
		if (number != expected) {
			printf("Expected return from zinquire(ifltab, \"FVER\") to equal %d, but got %d\n", expected, number);
			status = -1;
		}

		zclose(ifltab);
		remove(filename);
		return status;
	}
}

int testPseudoEightHourIrregular() {
	const char* filenames[] = { "PseudoEightHourIrregular.dss", "Output/PseudoEightHourIrregular.dss", "../bin/PseudoEightHourIrregular.dss" };
	char const* filename;
	const char* pathname = "//AARK/Flow/01Jan2018/~8Hour/KS2/";
	const char* expectedDateTimes[] = {
		"09Aug2018 0800",
		"09Aug2018 1600",
		"10Aug2018 0000",
		"10Aug2018 0800",
		"10Aug2018 1600",
		"11Aug2018 0000",
		"11Aug2018 0800",
		"11Aug2018 1600"
	};
	const float expectedValues[] = {100, 200, 300, 400, 500, 600, 700, 800};
	const int expectedValueCount = sizeof(expectedValues) / sizeof(expectedValues[0]);
	char dateStr[24];
	char timeStr[16];
	char dateTimeStr[40];
	long long ifltab[250] = { 0 };
	int status = 0;
	zStructTimeSeries* tss;
	FILE* f;
	for (int i = 0; i < sizeof(filenames) / sizeof(filenames[0]); ++i) {
		f = fopen(filenames[i], "r");
		if (f) {
			filename = filenames[i];
			break;
		}
	}
	if (f) {
		fclose(f);
		status = hec_dss_zopen(ifltab, filename);
		if (status != STATUS_OKAY) {
			fprintf(stderr, "Error opening file %s\n", filename);
			return status;
		}
		tss = zstructTsNew(pathname);
		status = ztsRetrieve(ifltab, tss, -1, 1, 0); // Floats
		if (status != STATUS_OKAY) {
			fprintf(stderr, "Error reading record %s\n", pathname);
			zclose(ifltab);
			return status;
		}
		printf("\nData as floats:\n");
		if (tss->numberValues != expectedValueCount) {
			fprintf(stderr, "Expected %d floats, but got %d\n", expectedValueCount, tss->numberValues);
			status = -1;
		}
		for (int i = 0; i < tss->numberValues; ++i) {
			minsToDateTime(tss->julianBaseDate * 1440 + tss->times[i], dateStr, timeStr, sizeof(dateStr), sizeof(timeStr));
			sprintf(dateTimeStr, "%s %s", dateStr, timeStr);
			if (strcmp(expectedDateTimes[i], dateTimeStr)) {
				fprintf(stderr, "Expected date/time of %s, but got %s\n", expectedDateTimes[i], dateTimeStr);
				status = -1;
			}
			if (tss->floatValues[i] != expectedValues[i]) {
				fprintf(stderr, "Expected value of %f, but got %f\n", expectedValues[i], tss->floatValues[i]);
				status = -1;
			}
			printf("\t%2d: %s %s\t%f\n", i, dateStr, timeStr, tss->floatValues[i]);
		}
		zstructFree(tss);
		tss = zstructTsNew(pathname);
		status = ztsRetrieve(ifltab, tss, -1, 2, 0); // Doubles
		if (status != STATUS_OKAY) {
			fprintf(stderr, "Error reading record %s\n", pathname);
			zclose(ifltab);
			return status;
		}
		printf("\nData as doubles:\n");
		if (tss->numberValues != expectedValueCount) {
			fprintf(stderr, "Expected %d doubles, but got %d\n", expectedValueCount, tss->numberValues);
			status = -1;
		}
		for (int i = 0; i < tss->numberValues; ++i) {
			minsToDateTime(tss->julianBaseDate * 1440 + tss->times[i], dateStr, timeStr, sizeof(dateStr), sizeof(timeStr));
			sprintf(dateTimeStr, "%s %s", dateStr, timeStr);
			if (strcmp(expectedDateTimes[i], dateTimeStr)) {
				fprintf(stderr, "Expected date/time of %s, but got %s\n", expectedDateTimes[i], dateTimeStr);
				status = -1;
			}
			if (tss->doubleValues[i] != expectedValues[i]) {
				fprintf(stderr, "Expected value of %f, but got %lf\n", expectedValues[i], tss->doubleValues[i]);
				status = -1;
			}
			printf("\t%2d: %s %s\t%f\n", i, dateStr, timeStr, tss->doubleValues[i]);
		}
		zstructFree(tss);
		zclose(ifltab);
	}
	else {
		char* dirname = getcwd(NULL, 0);
		for (int i = 0; i < sizeof(filenames) / sizeof(filenames[0]); ++i) {
			fprintf(stderr, "File %s does not exist in directory %s.\n", filenames[i], dirname);
		}
		free(dirname);
		status = 1;
	}
	return status;
}

int testOddNumberValues() {
	const char* filenames[] = { "OddNumberValues.dss", "Output/OddNumberValues.dss", "../bin/OddNumberValues.dss" };
	char const* filename;
	const char* pathnames[] = { "//AARK/Flow/01Jan2018/~8Hour/KS2/", "//AARK/Flow/01Aug2018/8Hour/KS2/" };
	const char* expectedDateTimes[] = {
		"09Aug2018 0800",
		"09Aug2018 1600",
		"10Aug2018 0000",
		"10Aug2018 0800",
		"10Aug2018 1600",
		"11Aug2018 0000",
		"11Aug2018 0800"
	};
	const float expectedValues[] = {100, 200, 300, 400, 500, 600, 700};
	const int expectedValueCount = sizeof(expectedValues) / sizeof(expectedValues[0]);
	char dateStr[24];
	char timeStr[16];
	char dateTimeStr[40];
	long long ifltab[250] = { 0 };
	int status = 0;
	zStructTimeSeries* tss;
	FILE* f;
	for (int i = 0; i < sizeof(filenames) / sizeof(filenames[0]); ++i) {
		f = fopen(filenames[i], "r");
		if (f) {
			filename = filenames[i];
			break;
		}
	}
	if (f) {
		fclose(f);
		status = hec_dss_zopen(ifltab, filename);
		if (status != STATUS_OKAY) {
			fprintf(stderr, "Error opening file %s\n", filename);
			return status;
		}
		for (int j = 0 ; j < sizeof(pathnames) / sizeof(pathnames[0]); ++j) {
			tss = zstructTsNew(pathnames[j]);
			status = ztsRetrieve(ifltab, tss, -1, 1, 0); // Floats
			if (status != STATUS_OKAY) {
				fprintf(stderr, "Error reading record %s\n", pathnames[j]);
				zclose(ifltab);
				return status;
			}
			printf("\nData as floats:\n");
			if (tss->numberValues != expectedValueCount) {
				fprintf(stderr, "Expected %d floats, but got %d\n", expectedValueCount, tss->numberValues);
				status = -1;
			}
			for (int i = 0; i < tss->numberValues; ++i) {
				minsToDateTime(tss->julianBaseDate * 1440 + tss->times[i], dateStr, timeStr, sizeof(dateStr), sizeof(timeStr));
				sprintf(dateTimeStr, "%s %s", dateStr, timeStr);
				if (strcmp(expectedDateTimes[i], dateTimeStr)) {
					fprintf(stderr, "Expected date/time of %s, but got %s\n", expectedDateTimes[i], dateTimeStr);
					status = -1;
				}
				if (tss->floatValues[i] != expectedValues[i]) {
					fprintf(stderr, "Expected value of %f, but got %f\n", expectedValues[i], tss->floatValues[i]);
					status = -1;
				}
				printf("\t%2d: %s %s\t%f\n", i, dateStr, timeStr, tss->floatValues[i]);
			}
			zstructFree(tss);
			tss = zstructTsNew(pathnames[j]);
			status = ztsRetrieve(ifltab, tss, -1, 2, 0); // Doubles
			if (status != STATUS_OKAY) {
				fprintf(stderr, "Error reading record %s\n", pathnames[j]);
				zclose(ifltab);
				return status;
			}
			printf("\nData as doubles:\n");
			if (tss->numberValues != expectedValueCount) {
				fprintf(stderr, "Expected %d doubles, but got %d\n", expectedValueCount, tss->numberValues);
				status = -1;
			}
			for (int i = 0; i < tss->numberValues; ++i) {
				minsToDateTime(tss->julianBaseDate * 1440 + tss->times[i], dateStr, timeStr, sizeof(dateStr), sizeof(timeStr));
				sprintf(dateTimeStr, "%s %s", dateStr, timeStr);
				if (strcmp(expectedDateTimes[i], dateTimeStr)) {
					fprintf(stderr, "Expected date/time of %s, but got %s\n", expectedDateTimes[i], dateTimeStr);
					status = -1;
				}
				if (tss->doubleValues[i] != expectedValues[i]) {
					fprintf(stderr, "Expected value of %f, but got %lf\n", expectedValues[i], tss->doubleValues[i]);
					status = -1;
				}
				printf("\t%2d: %s %s\t%f\n", i, dateStr, timeStr, tss->doubleValues[i]);
			}
			zstructFree(tss);
		}
		zclose(ifltab);
	}
	else {
		char* dirname = getcwd(NULL, 0);
		for (int i = 0; i < sizeof(filenames) / sizeof(filenames[0]); ++i) {
			fprintf(stderr, "File %s does not exist in directory %s.\n", filenames[i], dirname);
		}
		free(dirname);
		status = 1;
	}
	return status;
}

int testTextTableIssue135() {
	long long ifltab[250] = { 0 };
	int status = 0;
	const char* filename = "TextTableIssue135.dss";
	const char* pathname = "/a/b/Table/d/e/f/";
	const char* labelData[2] = { "alpha", "numeric" };
	const char* tableData[3][2] = { {"cat", "1"}, {"dog", "2"}, {"horse", "3"} };
	int numberLabelChars = 0;
	int numberTableChars = 0;
	int numberColumns = sizeof(labelData) / sizeof(labelData[0]);
	int numberRows = sizeof(tableData) / sizeof(tableData[0]);;
	char* cp;

	zStructText* ts = zstructTextNew(pathname);
	ts->numberColumns = numberColumns;
	ts->numberRows = numberRows;
	for (int i = 0; i < numberColumns; ++i) {
		ts->numberLabelChars += strlen(labelData[i]) + 1;
	}
	numberLabelChars = ts->numberLabelChars;
	for (int i = 0; i < numberRows; ++i) {
		for (int j = 0; j < numberColumns; ++j) {
			ts->numberTableChars += strlen(tableData[i][j]) + 1;
		}
	}
	numberTableChars = ts->numberTableChars;
	ts->labels = calloc(ts->numberLabelChars, sizeof(char));
	if (!ts->labels) {
		printf("Cannot allocate memory!\n");
		zstructFree(ts);
		return -1;
	}
	ts->allocated[zSTRUCT_TX_labels] = 1;
	cp = ts->labels;
	for (int i = 0; i < numberColumns; ++i) {
		strcpy(cp, labelData[i]);
		cp += strlen(cp) + 1;
	}
	ts->textTable = calloc(ts->numberTableChars, sizeof(char));
	if (!ts->textTable) {
		printf("Cannot allocate memory!\n");
		zstructFree(ts);
		return -1;
	}
	ts->allocated[zSTRUCT_TX_textTable] = 1;
	cp = ts->textTable;
	for (int i = 0; i < numberRows; ++i) {
		for (int j = 0; j < numberColumns; ++j) {
			strcpy(cp, tableData[i][j]);
			cp += strlen(cp) + 1;
		}
	}
	remove(filename);
	status = hec_dss_zopen(ifltab, filename);
	if (status != STATUS_OKAY) {
		printf("Error opening file %s\n", filename);
		zstructFree(ts);
		remove(filename);
		return status;
	}
	printf("STORING DATA:\n");

	printf("\tSPECIFIED TABLE\n\t");
	for (int i = 0; i < numberColumns; ++i) printf("\t%s", labelData[i]);
	printf("\n\t");
	for (int i = 0; i < numberColumns; ++i) printf("\t----");
	printf("\n");
	for (int i = 0; i < numberRows; ++i) {
		printf("\t");
		for (int j = 0; j < numberColumns; ++j) printf("\t%s", tableData[i][j]);
		printf("\n");
	}
	printf("\tENCODED TABLE\n");
	printf("\t\tnumber columns     = %d\n", ts->numberColumns);
	printf("\t\tnumber rows        = %d\n", ts->numberRows);
	printf("\t\tnumber label chars = %d\n", ts->numberLabelChars);
	printf("\t\tnumber table chars = %d\n", ts->numberTableChars);
	printf("\t");
	if (ts->numberLabelChars) {
		cp = ts->labels;
		int col = 0;
		while (cp - ts->labels < ts->numberLabelChars) {
			if (strcmp(cp, labelData[col++])) status = -1;
			printf("\t%s", cp);
			cp += strlen(cp) + 1;
		}
	}
	printf("\n\t");
	for (int i = 0; i < ts->numberColumns; ++i) printf("\t----");
	printf("\n\t");
	if (ts->numberTableChars) {
		cp = ts->textTable;
		int col = 0;
		int row = 0;
		while (cp - ts->textTable < ts->numberTableChars) {
			if (strcmp(cp, tableData[row][col])) status = -1;
			printf("\t%s", cp);
			cp += strlen(cp) + 1;
			if (++col % ts->numberColumns == 0) {
				col = 0;
				++row;
				printf("\n\t");
			}
		}
	}
	if (status != 0) {
		printf("Error encoding text table\n");
		zstructFree(ts);
		zclose(ifltab);
		remove(filename);
		return -1;
	}

	status = ztextStore(ifltab, ts);
	if (status != STATUS_OKAY) {
		printf("Error saving text table\n");
		zstructFree(ts);
		zclose(ifltab);
		remove(filename);
		return -1;
	}
	zstructFree(ts);
	ts = zstructTextNew(pathname);
	status = ztextRetrieve(ifltab, ts);
	if (status != STATUS_OKAY) {
		printf("Error retrieving text table\n");
		zstructFree(ts);
		zclose(ifltab);
		remove(filename);
		return -1;
	}
	printf("RETRIEVED DATA:\n");

	if (ts->numberColumns    != numberColumns) status = -1;
	if (ts->numberRows       != numberRows) status = -1;
	if (ts->numberLabelChars != numberLabelChars) status = -1;
	if (ts->numberTableChars != numberTableChars) status = -1;

	printf("\tENCODED TABLE\n");
	printf("\t\tnumber columns     = %d\n", ts->numberColumns);
	printf("\t\tnumber rows        = %d\n", ts->numberRows);
	printf("\t\tnumber label chars = %d\n", ts->numberLabelChars);
	printf("\t\tnumber table chars = %d\n", ts->numberTableChars);
	printf("\t");
	if (ts->numberLabelChars) {
		cp = ts->labels;
		int col = 0;
		while (cp - ts->labels < ts->numberLabelChars) {
			if (strcmp(cp, labelData[col++])) status = -1;
			printf("\t%s", cp);
			cp += strlen(cp) + 1;
		}
	}
	printf("\n\t");
	for (int i = 0; i < ts->numberColumns; ++i) printf("\t----");
	printf("\n\t");
	if (ts->numberTableChars) {
		cp = ts->textTable;
		int col = 0;
		int row = 0;
		while (cp - ts->textTable < ts->numberTableChars) {
			if (strcmp(cp, tableData[row][col])) status = -1;
			printf("\t%s", cp);
			cp += strlen(cp) + 1;
			if (++col % ts->numberColumns == 0) {
				col = 0;
				++row;
				printf("\n\t");
			}
		}
	}

	zstructFree(ts);
	zclose(ifltab);
	remove(filename);
	return status;
}

int testDss178() {
	const char* v7Filenames[] = { "DSS-178.dss", "Output/DSS-178.dss", "../bin/DSS-178.dss" };
	char v6Filename[_MAX_PATH];
	FILE* fp = NULL;
	int i;
	int status = -1;
	for (i = 0; i < sizeof(v7Filenames) / sizeof(v7Filenames[0]); ++i) {
		fp = fopen(v7Filenames[i], "r");
		if (fp) break;
	}
	if (fp) {
		fclose(fp);
		strcpy(v6Filename, v7Filenames[i]);
		v6Filename[strlen(v6Filename)-4] = '\0';
		strcat(v6Filename, "v6.dss");
		status = zconvertVersion(v7Filenames[i], v6Filename);
		remove(v6Filename);
	}
	return status;
}

int testTsStoreRules() {
            //---------------------//
            // build the data sets //
            //---------------------//
                                   //
                                   // Irregular store rule 0 doesn't work as the documentation suggests. Documentation indicates
                                   // that the existing values remain and only incoming values at non-existing times are stored.
                                   // In actuality, all incoming values are stored (even if UNDEFINED_DOUBLE - e.g., missing) and
                                   // the only existing values that remain are those with times not in the incoming values.
                                   //
                                   //                                                     *** Data Sets ***
                                   // +-------------------------------------------------- existing values
                                   // |       +------------------------------------------ incoming values
                                   // |       |                                           *** Regular TS Store Rules ***
                                   // |       |       +---------------------------------- replace all values                              (REPLACE ALL)
                                   // |       |       |       +-------------------------- replace missing values only                     (REPLACE MISSING VALUES ONLY)
                                   // |       |       |       |       +------------------ don't replace value with missing value          (REPLACE WITH NON MISSING)
                                   // |       |       |       |       |                   *** Irregular TS Store Rules ***
                                   // |       |       |       |       |       +---------- don't replace value with missing value          (REPLACE WITH NON MISSING)
                                   // |       |       |       |       |       |       +-- delete old values in time window before storing (DELETE INSERT)
            const char* data =     // |       |       |       |       |       |       |
                "Time               Old     New   Reg:0   Reg:1   Reg:4   Irr:0   Irr:1  \n"
                "01Jan2023 1:00      1      101     101       1     101     101     101  \n"
                "01Jan2023 2:00      2      102     102       2     102     102     102  \n"
                "01Jan2023 3:00      ~      103     103     103     103     103     103  \n"
                "01Jan2023 4:00      4        ~       ~       4       4       4       ~  \n"
                "01Jan2023 5:00      5      105     105       5     105     105     105  \n"
                "01Jan2023 6:00      ~      106     106     106     106     106     106  \n"
                "01Jan2023 7:00      7      107     107       7     107     107     107  \n"
                "01Jan2023 8:00      8        ~       ~       8       8       8       ~  \n"
                "01Jan2023 9:00      ~      109     109     109     109     109     109  \n"
                "01Jan2023 10:00    10      110     110      10     110     110     110  \n"
                "01Jan2023 11:00    11      111     111      11     111     111     111  \n"
                "01Jan2023 12:00     ~        ~       ~       ~       ~       ~       ~  \n"
                "01Jan2023 13:00    13      113     113      13     113     113     113  \n"
                "01Jan2023 14:00    14      114     114      14     114     114     114  \n"
                "01Jan2023 15:00     ~      115     115     115     115     115     115  \n"
                "01Jan2023 16:00    16        ~       ~      16      16      16       ~  \n"
                "01Jan2023 17:00    17      117     117      17     117     117     117  \n"
                "01Jan2023 18:00     ~      118     118     118     118     118     118  \n"
                "01Jan2023 19:00    19      119     119      19     119     119     119  \n"
                "01Mar2023 1:00     20      120     120      20     120     120     120  \n";
	char* line = NULL;
	char* element = NULL;
	char* saveptr1 = NULL;
	char* saveptr2 = NULL;
	int numData = 0;

	char* dataCopy = strdup(data);
	line = strtok_r(dataCopy, "\n", &saveptr1);
	while (line) {
		if (line && strstr(line, "Time") != line) {
			++numData;
		}
		line = strtok_r(NULL, "\n", &saveptr1);
	}
	char** dates = (char**)calloc(numData, sizeof(char*));
	char** times = (char**)calloc(numData, sizeof(char*));
	double* oldVals = (double*)calloc(numData, sizeof(double));
	double* newVals = (double*)calloc(numData, sizeof(double));
	double* expctedReg0 = (double*)calloc(numData, sizeof(double));
	double* expctedReg1 = (double*)calloc(numData, sizeof(double));
	double* expctedReg4 = (double*)calloc(numData, sizeof(double));
	double* expctedIrr0 = (double*)calloc(numData, sizeof(double));
	double* expctedIrr1 = (double*)calloc(numData, sizeof(double));
	int i = 0;
	dataCopy = strdup(data);
	line = strtok_r(dataCopy, "\n", &saveptr1);
	while (line) {
		if (strstr(line, "Time")  != line) {
			element = strtok_r(line, " ", &saveptr2);
			for (int j = 0; j < 9; ++j) {
				switch (j) {
				case 0: dates[i] = strdup(element); break;
				case 1: times[i] = strdup(element); break;
				case 2: oldVals[i] = element[0] == '~' ? UNDEFINED_DOUBLE : strtod(element, NULL); break;
				case 3: newVals[i] = element[0] == '~' ? UNDEFINED_DOUBLE : strtod(element, NULL); break;
				case 4: expctedReg0[i] = element[0] == '~' ? UNDEFINED_DOUBLE : strtod(element, NULL); break;
				case 5: expctedReg1[i] = element[0] == '~' ? UNDEFINED_DOUBLE : strtod(element, NULL); break;
				case 6: expctedReg4[i] = element[0] == '~' ? UNDEFINED_DOUBLE : strtod(element, NULL); break;
				case 7: expctedIrr0[i] = element[0] == '~' ? UNDEFINED_DOUBLE : strtod(element, NULL); break;
				case 8: expctedIrr1[i] = element[0] == '~' ? UNDEFINED_DOUBLE : strtod(element, NULL); break;
				}
				element = strtok_r(NULL, " ", &saveptr2);
			}
			++i;
		}
		line = strtok_r(NULL, "\n", &saveptr1);
	}
	//--------------//
	// do the tests //
	//--------------//
	int status = -1;
	zset("MLVL", "", 1);
	for (int dssVer = 6; dssVer <= 7; ++dssVer) {
		printf("DSS Version %d\n", dssVer);
		long long ifltab[250] = { 0 };
		char* dssFilename = strdup("testCwms1424_v?.dss");
		for (char* cp = dssFilename; *cp; ++cp) {
			if (*cp == '?') *cp = '0' + dssVer;
		}
		remove(dssFilename);
		status = dssVer == 6 ? zopen6(ifltab, dssFilename) : zopen7(ifltab, dssFilename);
		assert(status == STATUS_OKAY);
		zStructTimeSeries * tss = NULL;
		{
			printf("\tRegular time series\n");
			//--------------------------------------//
			// test regular time series store rules //
			//--------------------------------------//
			const char* pathname = "//StoreRuleTestLoc/Code//1Hour/StoreRuleTestTest/";
			char* missingRecordname = strdup("//StoreRuleTestLoc/Code/01Feb2023/1Hour/StoreRuleTestTest/");
			int storeRules[] = { 0,1,4 };
			int startJul = dateToJulian(dates[0]);
			int startSecs = timeStringToSeconds(times[0]);
			int endJul = dateToJulian(dates[numData - 1]);
			int endSecs = timeStringToSeconds(times[numData - 1]);
			int numVals = numberPeriods(SECS_IN_1_HOUR, startJul, startSecs, endJul, endSecs) + 1;
			double* oldValues = (double*)calloc(numVals, sizeof(double));
			double* newValues = (double*)calloc(numVals, sizeof(double));
			double* expectedValues = (double*)calloc(numVals, sizeof(double));
			int valJul;
			int valSecs;
			int dataJul;
			int dataSecs;
			//------------------------//
			// prepare the old values //
			//------------------------//
			valJul = startJul;
			valSecs = startSecs;
			dataJul = dateToJulian(dates[1]);
			dataSecs = timeStringToSeconds(times[1]);
			oldValues[0] = oldVals[0];
			for (int i = 1, j = 1; i < numVals; ++i) {
				incrementTime(SECS_IN_1_HOUR, 1, valJul, valSecs, &valJul, &valSecs);
				if (valJul < dataJul || (valJul == dataJul && valSecs < dataSecs)) {
					oldValues[i] = UNDEFINED_DOUBLE;
				}
				else {
					oldValues[i] = oldVals[j++];
					if (i < numVals - 1) {
						dataJul = dateToJulian(dates[j]);
						dataSecs = timeStringToSeconds(times[j]);
					}
				}
			}
			for (int i = 0; i < sizeof(storeRules) / sizeof(storeRules[0]); ++i) {
				printf("\t\tStore Rule %d\n", storeRules[i]);
				//----------------------//
				// store the old values //
				//----------------------//
				tss = zstructTsNewRegDoubles(
					pathname,
					oldValues,
					numVals,
					dates[0],
					times[0],
					"N/A",
					"INST-VAL");
				if (storeRules[i] == 0) {
					//--------------------------------//
					// test missing record management //
					//--------------------------------//
					// should store entire missing record
					status = ztsStore(ifltab, tss, 2);
					assert(status == STATUS_OKAY);
					assert(zcheck(ifltab, missingRecordname) == STATUS_RECORD_FOUND);
					if (dssVer == 7) {
						// should remove entire missing record (doesn't work correctly in DSS 6)
						status = ztsStore(ifltab, tss, 3);
						assert(status == STATUS_OKAY);
						assert(zcheck(ifltab, missingRecordname) == STATUS_RECORD_NOT_FOUND);
					}
				}
				status = ztsStore(ifltab, tss, 0);
				assert(status == STATUS_OKAY);
				zstructFree(tss);
				//------------------------//
				// prepare the new values //
				//------------------------//
				valJul = startJul;
				valSecs = startSecs;
				dataJul = dateToJulian(dates[1]);
				dataSecs = timeStringToSeconds(times[1]);
				newValues[0] = newVals[0];
				for (int j = 1, k = 1; j < numVals; ++j) {
					incrementTime(SECS_IN_1_HOUR, 1, valJul, valSecs, &valJul, &valSecs);
					if (valJul < dataJul || (valJul == dataJul && valSecs < dataSecs)) {
						newValues[j] = UNDEFINED_DOUBLE;
					}
					else {
						newValues[j] = newVals[k++];
						if (j < numVals - 1) {
							dataJul = dateToJulian(dates[k]);
							dataSecs = timeStringToSeconds(times[k]);
						}
					}
				}
				//----------------------//
				// store the new values //
				//----------------------//
				tss = zstructTsNewRegDoubles(
					pathname,
					newValues,
					numVals,
					dates[0],
					times[0],
					"N/A",
					"INST-VAL");
				status = ztsStore(ifltab, tss, storeRules[i]);
				assert(status == STATUS_OKAY);
				zstructFree(tss);
				//----------------------//
				// retrieve the results //
				//----------------------//
				tss = zstructTsNewTimes(
					pathname,
					dates[0],
					times[0],
					dates[numData-1],
					times[numData-1]);
				status = ztsRetrieve(ifltab, tss, 0, 0, 1);
				assert(status == STATUS_OKAY);
				//-----------------------------//
				// prepare the expected values //
				//-----------------------------//
				valJul = startJul;
				valSecs = startSecs;
				dataJul = dateToJulian(dates[1]);
				dataSecs = timeStringToSeconds(times[1]);
				switch (storeRules[i]) {
				case 0: expectedValues[0] = expctedReg0[0]; break;
				case 1: expectedValues[0] = expctedReg1[0]; break;
				case 4: expectedValues[0] = expctedReg4[0]; break;
				}
				for (int j = 1, k = 1; j < numVals; ++j) {
					incrementTime(SECS_IN_1_HOUR, 1, valJul, valSecs, &valJul, &valSecs);
					if (valJul < dataJul || (valJul == dataJul && valSecs < dataSecs)) {
						expectedValues[j] = UNDEFINED_DOUBLE;
					}
					else {
						switch (storeRules[i]) {
						case 0: expectedValues[j] = expctedReg0[k++]; break;
						case 1: expectedValues[j] = expctedReg1[k++]; break;
						case 4: expectedValues[j] = expctedReg4[k++]; break;
						}
						if (j < numVals - 1) {
							dataJul = dateToJulian(dates[k]);
							dataSecs = timeStringToSeconds(times[k]);
						}
					}
				}
				//----------------------------------------------//
				// compare the results with the expected values //
				//----------------------------------------------//
				assert(tss->numberValues == numVals);
				for (int j = 0; j < tss->numberValues; ++j) {
					if (tss->doubleValues[j] != expectedValues[j]) {
						printf("\t\t\tValue %4d: Expected %5.0f, got %5.0f\n", j, expectedValues[j], tss->doubleValues[j]);
					}
					assert(tss->doubleValues[j] == expectedValues[j]);
				}
			}
			free(oldValues);
			free(newValues);
			free(expectedValues);
			free(missingRecordname);
		}
		{
			printf("\tIrregular time series with gaps for missing\n");
			//----------------------------------------//
			// test irregular time series store rules //
			//----------------------------------------//
			const char* pathname = "//StoreRuleTestLoc/Code//~1Hour/StoreRuleTestTest/";
			int* timeVals = (int*)calloc(numData, sizeof(int));
			double* values = (double*)calloc(numData, sizeof(double));
			int numVals = 0;
			int jul;
			int secs;
			//-------------------------//
			// prepoare the old values //
			//-------------------------//
			for (int i = 0; i < numData; ++i) {
				if (oldVals[i] != UNDEFINED_DOUBLE) {
					jul = dateToJulian(dates[i]);
					secs = timeStringToSeconds(times[i]);
					timeVals[numVals] = jul * 1440 + secs / 60;
					values[numVals] = oldVals[i];
					++numVals;
				}
			}
			for (int storeRule = 0; storeRule <= 1; ++storeRule) {
				printf("\t\tStore Rule %d\n", storeRule);
				//----------------------//
				// store the old values //
				//----------------------//
				tss = zstructTsNewIrregDoubles(
					pathname,
					values,
					numVals,
					timeVals,
					SECS_IN_1_MINUTE,
					NULL,
					"N/A",
					"INST-VAL");
				status = ztsStore(ifltab, tss, 0);
				assert(status == STATUS_OKAY);
				zstructFree(tss);
				//-------------------------//
				// prepoare the new values //
				//-------------------------//
				numVals = 0;
				for (int i = 0; i < numData; ++i) {
					if (newVals[i] != UNDEFINED_DOUBLE) {
						jul = dateToJulian(dates[i]);
						secs = timeStringToSeconds(times[i]);
						timeVals[numVals] = jul * 1440 + secs / 60;
						values[numVals] = newVals[i];
						++numVals;
					}
				}
				//----------------------//
				// store the new values //
				//----------------------//
				tss = zstructTsNewIrregDoubles(
					pathname,
					values,
					numVals,
					timeVals,
					SECS_IN_1_MINUTE,
					NULL,
					"N/A",
					"INST-VAL");
				status = ztsStore(ifltab, tss, storeRule);
				assert(status == STATUS_OKAY);
				zstructFree(tss);
				//----------------------//
				// retrieve the results //
				//----------------------//
				tss = zstructTsNewTimes(
					pathname,
					dates[0],
					times[0],
					dates[numData - 1],
					times[numData - 1]);
				status = ztsRetrieve(ifltab, tss, 0, 0, 1);
				assert(status == STATUS_OKAY);
				//-----------------------------//
				// prepare the expected values //
				//-----------------------------//
				numVals = 0;
				for (int i = 0; i < numData; ++i) {
					if ((storeRule == 0 && expctedIrr0[i] != UNDEFINED_DOUBLE) || (storeRule == 1 && expctedIrr1[i] != UNDEFINED_DOUBLE)) {
						jul = dateToJulian(dates[i]);
						secs = timeStringToSeconds(times[i]);
						timeVals[numVals] = jul * 1440 + secs / 60;
						values[numVals] = storeRule == 0 ? expctedReg4[i] : expctedIrr1[i];
						++numVals;
					}
				}
				//----------------------------------------------//
				// compare the results with the expected values //
				//----------------------------------------------//
				assert(tss->numberValues == numVals);
				for (int j = 0; j < tss->numberValues; ++j) {
					if (tss->doubleValues[j] != values[j]) {
						printf("\t\t\tValue %4d: Expected %5.0f, got %5.0f\n", j, values[j], tss->doubleValues[j]);
						zclose(ifltab);
					}
					assert(tss->doubleValues[j] == values[j]);
				}
				zstructFree(tss);
			}
			free(timeVals);
			free(values);
		}
		{
			printf("\tIrregular time series with UNDEFINED_DOUBLE for missing\n");
			//----------------------------------------//
			// test irregular time series store rules //
			//----------------------------------------//
			const char* pathname = "//StoreRuleTestLoc/Code//~1Hour/StoreRuleTestTest/";
			int* timeVals = (int*)calloc(numData, sizeof(int));
			int jul;
			int secs;
			//-------------------------//
			// prepoare the old values //
			//-------------------------//
			for (int i = 0; i < numData; ++i) {
				jul = dateToJulian(dates[i]);
				secs = timeStringToSeconds(times[i]);
				timeVals[i] = jul * 1440 + secs / 60;
			}
			for (int storeRule = 0; storeRule <= 1; ++storeRule) {
				printf("\t\tStore Rule %d\n", storeRule);
				//----------------------//
				// store the old values //
				//----------------------//
				tss = zstructTsNewIrregDoubles(
					pathname,
					oldVals,
					numData,
					timeVals,
					SECS_IN_1_MINUTE,
					NULL,
					"N/A",
					"INST-VAL");
				status = ztsStore(ifltab, tss, 0);
				assert(status == STATUS_OKAY);
				zstructFree(tss);
				//----------------------//
				// store the new values //
				//----------------------//
				tss = zstructTsNewIrregDoubles(
					pathname,
					newVals,
					numData,
					timeVals,
					SECS_IN_1_MINUTE,
					NULL,
					"N/A",
					"INST-VAL");
				status = ztsStore(ifltab, tss, storeRule);
				assert(status == STATUS_OKAY);
				zstructFree(tss);
				//----------------------//
				// retrieve the results //
				//----------------------//
				tss = zstructTsNewTimes(
					pathname,
					dates[0],
					times[0],
					dates[numData - 1],
					times[numData - 1]);
				status = ztsRetrieve(ifltab, tss, 0, 0, 1);
				assert(status == STATUS_OKAY);
				//----------------------------------------------//
				// compare the results with the expected values //
				//----------------------------------------------//
				assert(tss->numberValues == numData);
				for (int j = 0; j < tss->numberValues; ++j) {
					if (tss->doubleValues[j] != expctedIrr1[j]) {
						printf("\t\t\tValue %4d: Expected %5.0f, got %5.0f\n", j, expctedIrr1[j], tss->doubleValues[j]);
						zclose(ifltab);
					}
					assert(tss->doubleValues[j] == expctedIrr1[j]);
				}
				zstructFree(tss);
			}
			free(timeVals);
		}
		zclose(ifltab);
		free(dssFilename);
	}
	//----------//
	// clean up //
	//----------//
	for (int i = 0; i < numData; ++i) {
		free(dates[i]);
		free(times[i]);
	}
	free(dates);
	free(times);
	free(oldVals);
	free(newVals);
	free(expctedReg0);
	free(expctedReg1);
	free(expctedReg4);
	free(expctedIrr0);
	free(expctedIrr1);
	return status;
}