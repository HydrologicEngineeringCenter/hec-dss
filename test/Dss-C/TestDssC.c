#include <stdio.h>
#include <string.h>

#include "heclib.h"
#include "hecdss7.h"
#include "TestDssC.h"


int runTheTests();



void usage(char* exeName)
{
	printf("Version: %s %s\n",__DATE__,__TIME__);
	
	printf("\nUsage:\n %s command [options] [dssfilename]  ", exeName);
	printf("\ncommands: test|catalog|zqueeze|lock seconds|check-lock|zcheck(File|Links|Pathnames) |workout version timeSeriesCount timeSeriesLength");
	printf("\nWhere:");
	printf("\ntest, runs standard set of DSS tests (needs to run in directory with test files.");
	printf("\ncatalog, prints the DSS catalog to the console");
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
	printf("\n%s catalog myfile.dss details     # includes record type in output", exeName);
	printf("\n%s zsqueeze myfile.dss", exeName);
	printf("\n%s lock 15 myfile.dss", exeName);
	printf("\n%s check-lock myfile.dss", exeName);
	printf("\n%s zcheckFile myfile.dss", exeName);
	printf("\n%s zcheckLinks myfile.dss", exeName);;
	printf("\n%s zcheckPathnames myfile.dss", exeName);
	printf("\n%s export myfile.dss /SHG/EFRUSSIAN20/PRECIPITATION/01OCT2004:2400/02OCT2004:0100/GAGEINTERP/ 1", exeName);
	printf("\n%s pathnameTesting file.dss 7", exeName);
	printf("\n%s import-profile input.csv output.dss //location1/id-depth//10Second/ADCIRC-run12/ 2022-05-04 10:12:30 feet INST-VAL",exeName);
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
	else if (strcmp(argv[1], "export") == 0 && ( argc == 4 || argc == 5)) { // export file.dss path metaDataOnly(0|1)
		int metaDataOnly = 0;
		if (argc == 5) metaDataOnly = strcmp(argv[4],"1")? 0:1;
		status= Export(argv[2], argv[3],metaDataOnly);
	}
	else if (argc == 3 && strcmp(argv[1], "recordinfo") == 0) {
		testRecordInfo6(argv[2]);
	}
	else if (argc == 4 && strcmp(argv[1], "pathnameTesting") == 0) {
		status = PathnameTesting(argv[2],atoi(argv[3]));
	}
	else if (argc == 9 && strcmp(argv[1], "import-profile") == 0){
		//int ImportProfile(const char* csvFilename, const char* dssFilename, const char* path, const char* date, const char* time, const char* units, const char* datatype);
		status = ImportProfile(argv[2], argv[3], argv[4], argv[5], argv[6], argv[7],argv[8]);
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
	long long ifltab7[250];
	long long ifltab6[250];
	char fileName7[80];
	char fileName7a[80];
	char fileName6[80];
	int status;

	printf("\ntest Unit\n");
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

	//status = zopen(ifltab7, "C:\\Users\\q0hecwjc\\Desktop\\funny.dss");
	//status = zopen(ifltab7, "C:\\Users\\q0hecwjc\\Desktop\\db7.dss");
	//status = zcatalog((long long*)ifltab7, catStruct->pathWithWildChars, catStruct, 0);

	//testCatalog();
	//testConversion();
	//zset("MLVL", "", 15);
	//stringCopy(fileName7, sizeof(fileName7), "C:/Users/q0hecwjc/Desktop/snow.2016.01.dss", sizeof(fileName7));	
	//stringCopy(fileName7, sizeof(fileName7), "C:/Users/q0hecwjc/Desktop/airtemp.2016.04.dss", sizeof(fileName7));	
	//stringCopy(fileName7, sizeof(fileName7), "C:/Users/q0hecwjc/Documents/test.dss", sizeof(fileName7));
	//status = zopen(ifltab7, fileName7);
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
	//if (status != STATUS_OKAY) return status;

	status = testMisc();
	if (status != STATUS_OKAY) return status;

	status = testDateTime();
	if (status != STATUS_OKAY) return status;

	status = testDelete("testUtilities7.dss", "testUtilities6.dss");
	if (status != STATUS_OKAY) return status;

	stringCopy(fileName7, sizeof(fileName7), "testDss7.dss", sizeof(fileName7));
	remove(fileName7);

	status = zopen(ifltab7, fileName7);
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
	status = zopen(ifltab7, fileName7);
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
	status = zopen(ifltab7, fileName7);
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
	status = zopen(ifltab7, fileName7);
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
	status = zopen(ifltab7, fileName7);
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
	int status = zopen(ifltab, "C:/temp/testarray.dss");

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
	status = zopen(ifltab, "C:/temp/testpd.dss");
	//zsetfi_(ifltab,"reclaim", " ", RECLAIM_NONE,&i,&status,7,1);

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

	int status = zopen(ifltab, "charlong_7.dss");
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
	tss1->floatValues = fvalues;
	tss1->units = "DEG C";
	tss1->type = "INST-VAL";

	long long ifltab[250];
	int status = zopen6(ifltab, dssFileName);
	if (status) return status;
	
	ztsStore(ifltab, tss1, 0);
	zstructFree(tss1);
	zclose(ifltab);

	zStructTimeSeries* tss2 = zstructTsNew(path);
	long long ifltab2[250];
	zopen(ifltab2, dssFileName);
	status = ztsRetrieve(ifltab2, tss2, -1, 1, 0);
	
	printf("\nunits= '%s'", tss2->units);
	printf("\ntype= '%s'", tss2->type);
	printf("\ntimeZoneName= '%s'", tss2->timeZoneName);
	
	printf("\n");
	
	const char* expectedUnits = "DEG C";

	if (strncmp(expectedUnits, tss2->units, strlen(expectedUnits))) {
		status = -2;
	}
	if (strncmp("INST-VAL", tss2->type, 8) !=0 )
		status = -3;

	zstructFree(tss2);
	zclose(ifltab2);

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

 
