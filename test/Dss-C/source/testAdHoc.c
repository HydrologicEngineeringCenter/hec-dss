//#include <Time.h>
//#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include "string.h"
//#include <Winsock2.h>
//#include <process.h>
//#include <tchar.h>
//#include <Psapi.h>
#include <math.h>

#include "zdssMessages.h"
#include "hecdss7.h"
#include "heclib.h"
#include "hecdssInternal.h"
#include "TestDssC.h"


#include "hecdss7.h"
//#  Note - No header included for def, as to avoid compiler errors!


int charLongxx(void *from, void *to, int numberBytes, int maxBytesTo, int zeroEndFlag)
{
	int i;
	int j;
	int len;
	int ipos;
	int done;
	int nlongwords;
	int numbWordsToZero;
	unsigned char *charFrom;
	unsigned char *charTo;
	unsigned long long *fromLong;
	unsigned long long imLong;


	charFrom = (unsigned char *)from;
	charTo = (unsigned char *)to;

	nlongwords = ((numberBytes - 1) / 8) + 1;
	if (!bigEndian()) {
		if (numberBytes > 0) {
			if (numberBytes > maxBytesTo) numberBytes = maxBytesTo;
			for (i = 0; i < numberBytes; i++) {
				charTo[i] = charFrom[i];
			}
		}
		else {
			numberBytes = 0;
			for (i = 0; i < maxBytesTo; i++) {
				charTo[numberBytes++] = charFrom[i];
				if (charFrom[i] == '\0') break;
			}
		}
	}
	else {

		//  On big endian machines, we need to swap characters
		//  This function is always used with 8 byte words, so we can swap safely
		fromLong = (unsigned long long*)from;
		if (numberBytes > 0) {
			if (numberBytes > maxBytesTo) numberBytes = maxBytesTo;
			ipos = 0;
			for (j = 0; j < nlongwords; j++) {
				imLong = fromLong[j];
				zswap(&imLong, 2);
				charFrom = (unsigned char *)&imLong;
				for (i = 0; i < 8; i++) {
					charTo[ipos++] = charFrom[i];
					if (ipos >= numberBytes) break;
				}
			}
		}
		else {
			numberBytes = 0;
			nlongwords = ((maxBytesTo - 1) / 8) + 1;
			done = 0;
			for (j = 0; j < nlongwords; j++) {
				imLong = fromLong[j];
				zswap(&imLong, 2);
				charFrom = (unsigned char *)&imLong;
				for (i = 0; i < 8; i++) {
					charTo[numberBytes++] = charFrom[i];
					if (charFrom[i] == '\0') done = 1;
					if (numberBytes >= maxBytesTo) done = 1;
					if (done) break;
				}
				if (done) break;
			}
		}
	}

	if (zeroEndFlag) {
		if (zeroEndFlag == 1) {
			numbWordsToZero = nlongwords;
		}
		else {
			numbWordsToZero = ((maxBytesTo - 1) / 8) + 1;
		}
		len = numbWordsToZero * 8;
		if (len > maxBytesTo) len = maxBytesTo;
		if (numberBytes < len) {
			for (i = numberBytes; i < len; i++) {
				charTo[i] = '\0';
			}
		}
	}

	return nlongwords;
}


char getCharacter(int i) {
	int j;


	j = i/26;
	i = i - (j * 26);

	i += 97;
	if ((i >= 97) && (i <= 122)) {
		return i;
	}
	return 97;
}

void pathStr(char *str, int pos) {
	int i, j;

	str[0] = '/';
	i = pos /22;
	j = pos - (i * 22);
	if (j < 0) j = -j;
	if (j < 5) j += 5;
	for (i=0; i<j; i++) {
		str[i+1] = getCharacter(pos+i);
	}
	str[j+1] = '\0';
}

int testAdHoc2()
{
	long long ifltab7[250];
	char fileName7[80];
	long long ifltab6[250];
	char name[200];
	char *path;
	int status;
	int i;
	char cdate[20];
	char ctime1[20];
	zStructTimeSeries *tss1, *tss2;
	zStructTransfer *ztrans;
	int hi, low, code;

	double vals[] = { 1,2,3 };
	int times[] = { 57900960,
		58000960,
		58900960 };
	double values1[] = { 3,4 };
	int times1[] = {
		58000960,
		58900960 };

	//charLong(void *from, void *to, int numberBytes, int maxBytesTo, int zeroEndFlag)
	//strncpy_s((char *)ifltab7, sizeof(fileName7), "testDss7.dss", sizeof(fileName7));
	//charLong(ifltab7, fileName7, 0, 50, 1);

	//strncpy_s(fileName7, sizeof(fileName7), "C:/Users/q0hecwjc/Desktop/weather_orig2.dss", sizeof(fileName7));
	stringCopy(fileName7, sizeof(fileName7), "test7x.dss", sizeof(fileName7));
	status = hec_dss_zopen(ifltab7, fileName7);
	if (status) return status;

	//zsetMessageLevel(MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_2);
	//zset("mlvl", "", 15);
	//testztsStruct1(ifltab7);


	path = mallocAndCopy("/TEST/TEST/FLOW//IR-Day/TEST8x/");
	//zsetMessageLevel(MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_2);
	zsetMessageLevel(MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG);
	//status = ztsGetDateRange(ifltab7, path, 1, &juls, &jule);

/*	zcheckFile(ifltab7);

	

/*	strncpy_s(fileName7, sizeof(fileName7), "sample7.dss", sizeof(fileName7));
	status = hec_dss_zopen(ifltab7, fileName7);
	if (status) return status;
	status = zdelete(ifltab7, "/GREEN RIVER/GLENFIR/FLOW/01Apr1992/1Hour/OBS/");


	path = mallocAndCopy("//BLOOMSBURG/FLOW//IR-MONTH/ESTIMATED SAME DAY/");
	strncpy_s(fileName7, sizeof(fileName7), "C:/Users/q0hecwjc/Desktop/Trinity_0525_25K_Events.dss", sizeof(fileName7));
	status = hec_dss_zopen(ifltab7, fileName7);
	if (status) return status;
	zopenExtended(long long *ifltab, const char *dssFilename, int fileVersion,
	int access, int maxExpectedPathnames, int hashSize, int binSize);
	

	strncpy_s(fileName7, sizeof(fileName7), "C:/Users/q0hecwjc/Desktop/Trinity_0525_25K_Events.dss", sizeof(fileName7));
	status = zopenExtended(ifltab7, fileName7, 7, 0, 0, 8192, 200);
	zset("mlvl", "", 1);
	zcheckLinks(ifltab7);
	zclose(ifltab7);




	zanalyzeHashTable(ifltab7);

	catStruct = zstructCatalogNew();
	zset("mlvl", "", 15);
	status = zcatalog(ifltab7, (const char *)0, catStruct, 0);
	zstructTsNewIrregFloats(const char* pathname, float *floatValues, int numberValues,
	*										 int *itimes, int timeGranularitySeconds, const char* startDateBase,
	*										 const char *units, const char *type);
	*/

/*	tss1 = zstructTsNewIrregDoubles(path, vals, 3, times, 60, "", "cfs", "inst-val");
	status = ztsStore(ifltab7, tss1, 1);

	tss1 = zstructTsNew(path);
	tss1->boolRetrieveAllTimes = 1;
	status = ztsRetrieve(ifltab7, tss1, 0, 0, 0);

	// Print out.  (Compute time for each ordinate, values are floats)		
	for (i = 0; i<tss1->numberValues; i++) {
		getDateAndTime(tss1->times[i], tss1->timeGranularitySeconds, tss1->julianBaseDate,
			cdate, sizeof(cdate), ctime1, sizeof(ctime1));
		printf("Oridnate %d, for %s, %s, value is %f\n", i, cdate, ctime1, tss1->doubleValues[i]);
	}
*/
	tss2 = zstructTsNewIrregDoubles(path, values1, 2, times1, 60, "", "cfs", "inst-val");
	tss2->startJulianDate = 40209;
	tss2->startTimeSeconds = 38400;
	tss2->endJulianDate = 40904;
	tss2->endTimeSeconds = 38400; 
	status = ztsStore(ifltab7, tss2, 1);


	tss1 = zstructTsNew(path);
	tss1->boolRetrieveAllTimes = 1;
	status = ztsRetrieve(ifltab7, tss1, 0, 0, 0);

	// Print out.  (Compute time for each ordinate, values are floats)		
	for (i = 0; i<tss1->numberValues; i++) {
		getDateAndTime(tss1->times[i], tss1->timeGranularitySeconds, tss1->julianBaseDate,
			cdate, sizeof(cdate), ctime1, sizeof(ctime1));
		printf("Oridnate %d, for %s, %s, value is %f\n", i, cdate, ctime1, tss1->doubleValues[i]);		
	}


	path = mallocAndCopy("//FOLSOM-POOL/FLOW-OUT/01Jan2000/1Hour/C:000004|NOENSEMBLE3/");
	stringCopy(fileName7, sizeof(fileName7), "forBill/DSS_Failure_Files/2018_04_20/simulation.dss", sizeof(fileName7));
	status = hec_dss_zopen(ifltab7, fileName7);
	if (status) return status;

	ztrans = zstructTransferNew(path, 1);
	zread(ifltab7, ztrans);

	tss1 = zstructTsNew(path);
	//tss1->boolRetrieveAllTimes = 1;
	zset("MLVL", "", 15);
	//zrecordAddresses(ifltab7, tss1->pathname, addresses);
	//status = ztsRetrieve(ifltab7, tss1, 0, 0, 0);

	tss2 = zstructTsNewRegFloats(path, (float *)ztrans->values1, ztrans->values1Number/2, "01Jan2000", "0100", "cfs", "INST-VAL");

	status = hec_dss_zopen(ifltab6, "mydb7.dss");
	if (status) return status;
	status = ztsStore(ifltab6, tss2, 0);




	hi = 1;
	path = mallocAndCopy("30MINUTES");
	ztsGetStandardInterval(7, &low, path, strlen(path), &hi);


	stringCopy(fileName7, sizeof(fileName7), "C:/Users/q0hecwjc/Desktop/Reservoir Info.dss", sizeof(fileName7));
	status = hec_dss_zopen(ifltab6, "C:/Users/q0hecwjc/Desktop/Reservoir2.dss");
	if (status) return status;

	status = hec_dss_zopen(ifltab7, fileName7);
	if (status) return status;

	//zloadcache6_(ifltab7, &status);
	//zckpat6_(ifltab7, &status);
	//zset("MLVL", "", 15);
	//zcklnk6_(ifltab7, &status);
	//zckpnb6_(ifltab7, &status);


	//zcheckFile(ifltab7);

	path = mallocAndCopy("//Sayers Stor-flow/STORAGE-FLOW///TABLE/");

	status = zcopyRecord(ifltab7, ifltab6, path, path);

	return status;

	stringCopy(fileName7, sizeof(fileName7), "empty.dss", sizeof(fileName7));
	status = hec_dss_zopen(ifltab7, fileName7);
	if (status) return status;

	stringCopy(fileName7, sizeof(fileName7), "empty2.dss", sizeof(fileName7));
	status = hec_dss_zopen(ifltab6, fileName7);
	if (status) return status;


	zcopyfile_((long long*)ifltab7, (long long*)ifltab6, &status);

	status = zerrorDecode(-status, &hi, &low, &code, &i);
	path = zgetFunctionName(hi);
	path = zgetFunctionName(low);
	zerrorMessage(name, sizeof(name), status, code, low);

	stringCopy(fileName7, sizeof(fileName7), "sample7.dss", sizeof(fileName7));
	status = hec_dss_zopen(ifltab7, fileName7);
	if (status) return status;	

	zcheckFile(ifltab7);

	path = mallocAndCopy("//OLMSTED/WICKET1/13OCT2017/1MIN/T3/");
	zset("MLVL", "", 15);
	status = zcheck(ifltab7, path);
	status = zdelete(ifltab7, path);
	status = zcheck(ifltab7, path);
	zclose(ifltab7);
	hec_dss_zopen(ifltab7, fileName7);
	status = zcheck(ifltab7, path);

	tss2 = zstructTsNew(path);
	//tss2->boolRetrieveAllTimes = 1;
	status = ztsRetrieve(ifltab7, tss2, -1, 2, 0);

	ztsStore(ifltab7, tss2, 0);

	zstructFree(tss2);

	free(path);
	zclose(ifltab7);
	return status;
}


int testAdHoc()
{
	int DEBUG = 0;
	int status = 0;
	long long ifltab[250];
	long long ifltab2[250];
	float fvalues[2];
	int i, j;
	zStructTimeSeries *tss1, *tss2;
	zStructPairedData *pds;
	double dvalues[200];
	int itimes[200];
	int julian;
	int mins;
	long long bufferControl[4] = { 0,0,0,0 };
	int zero = 0;
	char cbuff[40];
	char *cpath;
	char *cnull = 0;
	int searchOption;
	int startJulian[1];
	int startMinutes[1];
	int endJulian[1];
	int exists[1];
	int endMinutes[1];
	zStructPairedData *pdsd1 = 0;
	char cpath1[200];
	char cpath2[100];
	char cpath3[100];
	char pathname[392];
	int secondsPastMidnight, millsPastSecond;
	// LARGE_INTEGER StartingTime, EndingTime, ElapsedMicroseconds;
	// LARGE_INTEGER Frequency;
	// HMODULE hMods[1024];
	// HANDLE hProcess;
	// DWORD cbNeeded;
	// LPWSTR szProcessName[200];
	// DWORD pid;
	// DWORD value = MAX_PATH;
	// TCHAR  buff[MAX_PATH];
	// DWORD dw;
	int userHeader[2], nUserHeader, numberChars;

	long long fileTime;

	fileTime = 12345;
	printf("Long number is %lld\n", fileTime);




	//getCurrentDateTime(&julian, &secondsPastMidnight, &millsPastSecond);
	//start = getCurrentTimeMillis();
	//unlink("C:/Users/q0hecwjc/Desktop/tt1.dss");
	//status = hec_dss_zopen(ifltab, "C:/Users/q0hecwjc/Desktop/S_Santian_R_CW1.dss");
	//zset("MLVL", "", 15);
	//status = hec_dss_zopen(ifltab2, "C:/Users/q0hecwjc/Desktop/tt1.dss");
	status = hec_dss_zopen(ifltab, "trim/test6.dss");
	tss1 = zstructTsNew("//KEYS/ELEV//1HOUR/OBS/");
	status = ztsRetrieve(ifltab, tss1, -1, 1, 1);
	if (status != STATUS_OKAY) {
		printf("\n**** error in ztsRetrieve  /Basin/Location/Flow/01Jan2001/1Hour/C Test/ ***\nStatus: %d\n", status);
	}
	/*status = zcopyFile(ifltab, ifltab2, 0);
	status = zcheckFile(ifltab);

//	zconvertVersion("C:/hmsproj7/c6.dss", "C:/hmsproj7/c7.dss");
//	zconvertVersion("C:/hmsproj7/c7.dss", "C:/hmsproj7/c77.dss");
//	zconvertVersion("C:/hmsproj7/c77.dss", "C:/hmsproj7/c66.dss");
/*
	zset("MLVL", "", 0);
	printCurrentTime(1);
	i = 0;

	QueryPerformanceCounter(&StartingTime);
	QueryPerformanceFrequency(&Frequency);

	printf("starting c6.dss to c7.dss\n");
	zconvertVersion("c6.dss", "c7.dss");
	printCurrentTime(1);
	QueryPerformanceCounter(&EndingTime);
	ElapsedMicroseconds.QuadPart = EndingTime.QuadPart - StartingTime.QuadPart;
	ElapsedMicroseconds.QuadPart *= 1000000;
	ElapsedMicroseconds.QuadPart /= Frequency.QuadPart;
	longWithCommas(cbuff, sizeof(cbuff), (long long)ElapsedMicroseconds.QuadPart);
	printf("%d, time = %s \n\n", i, cbuff);

	printf("starting c6.dss to c7.dss\n");
	QueryPerformanceCounter(&StartingTime);
	QueryPerformanceFrequency(&Frequency);
	zconvertVersion("c7.dss", "c77.dss");
	printCurrentTime(1);
	QueryPerformanceCounter(&EndingTime);
	ElapsedMicroseconds.QuadPart = EndingTime.QuadPart - StartingTime.QuadPart;
	ElapsedMicroseconds.QuadPart *= 1000000;
	ElapsedMicroseconds.QuadPart /= Frequency.QuadPart;
	longWithCommas(cbuff, sizeof(cbuff), (long long)ElapsedMicroseconds.QuadPart);
	printf("%d, time = %s \n\n", i, cbuff);

	printf("starting c6.dss to c7.dss\n");
	QueryPerformanceCounter(&StartingTime);
	QueryPerformanceFrequency(&Frequency);
	zconvertVersion("c77.dss", "c66.dss");
	printCurrentTime(1);
	QueryPerformanceCounter(&EndingTime);
	ElapsedMicroseconds.QuadPart = EndingTime.QuadPart - StartingTime.QuadPart;
	ElapsedMicroseconds.QuadPart *= 1000000;
	ElapsedMicroseconds.QuadPart /= Frequency.QuadPart;
	longWithCommas(cbuff, sizeof(cbuff), (long long)ElapsedMicroseconds.QuadPart);
	printf("%d, time = %s \n\n", i, cbuff);

	printf("done\n");

	return 0;

	catStruct = zstructCatalogNew();
	status = hec_dss_zopen(ifltab, "C:/Users/q0hecwjc/Desktop/Bald_Eagle_HMSFRA.dss");
	status = zcatalog((long long*)ifltab, catStruct->pathWithWildChars, catStruct, 0);
	if (zisError(status)) {
		return status;
	}
	
		for (i = 0; i<catStruct->numberPathnames; i++) {
			/////////////////////////////////////////////////
			//  For compatibility purposes only...
			//  Make pathname uppercase and change "Minute" to "MIN"
			upperCase(catStruct->pathnameList[i]);
			pos = strstr(catStruct->pathnameList[i], "MINUTE/");
			if (pos) {
				cpath = catStruct->pathnameList[i];
				len = strlen(cpath);
				pos += 3;
				j = pos - cpath;
				cpath[j] = '\0';
				pos += 3;
				strcat_s(cpath, len, pos);
			}
		}
	


	hec_dss_zopen(ifltab, "mydb6.dss");
	number = 15;
	zset6_("MLVL", " ", &number, 4, 1);
	number = 2;
	zsetfi6_(ifltab, "MULT", "ON", &number, &status, 4, 2);
	zmultu6_(ifltab, 1, 0);


	zsetfi_(ifltab, "FMULT", "ON", &number, &status,
		5, 2);
		*/
	//hec_dss_zopen(ifltab, (const char*)0);

	hec_dss_zopen(ifltab, "Sample7.dss");
	zset("MLVL", "", 2);

	//for (i = 0; i < 500000; i++) {
		tss1 = zstructTsNew("/Basin/Location/Flow/01Jan2010/1Hour/Java Sample/");
		tss1->locationStruct = zstructLocationNew(tss1->pathname);
		tss1->allocated[zSTRUCT_TS_locationStruct] = 1;
		status = ztsRetrieve(ifltab, tss1, -1, 1, 1);
		if (status) printf("Error, status = %d\n", status);
		zstructFree(tss1);
	//}
		zclose(ifltab);
	return 0;


	hec_dss_zopen(ifltab, "BasePORshortXSonlyFIA4b.dss");

	stringCopy(pathname, sizeof(pathname), "/RUSSIAN COYOTETODC/35.61/FLOW//1HOUR/TW-Tests_P:POR_Short:RAS-POR/", _TRUNCATE);
	tss1 = zstructTsNewTimes(pathname, "", "", "", "");
	ztsRetrieve(ifltab, tss1, -1, 1, 1);


	stringCopy(cpath1, sizeof(cpath1), "/This is a text message that is written to HEC-DSS", _TRUNCATE);


	numberChars = (int)strlen(cpath1);
	nUserHeader = 0;

	zstxta_((long long*)ifltab, pathname, cpath1, &numberChars,
		userHeader, &nUserHeader, &status,
		strlen(pathname), strlen(cpath1));

	/*
	hec_dss_zopen(ifltab, "forBill/SSP_Examples/SSP_EXAMPLES.dss");

	stringCopy(pathname, sizeof(pathname), "/CHATTAHOOCHEE RIVER/CORNELIA, GA/FLOW/01Jan1962/1Day/USGS/", _TRUNCATE);
	stringCopy(cpath1, sizeof(cpath1), "/CHATTAHOOCHEE RIVER/CORNELIA, GA/FLOW/01Jan1962/1Day/v5/", _TRUNCATE);
	zsetMessageLevel(9, 6);
	zcheckMultiUser((long long*)ifltab);
	zduplicateRecord(ifltab, pathname, cpath1);

	stringCopy(cpath1, sizeof(cpath1), "/CHATTAHOOCHEE RIVER/CORNELIA, GA/FLOW/01Jan1962/1Day/v6/", _TRUNCATE);
	zcheckMultiUser((long long*)ifltab);
	zcheckMultiUser((long long*)ifltab);
	zduplicateRecord(ifltab, pathname, cpath1);

	hec_dss_zopen(ifltab, "collections7.dss");
	stringCopy(pathname, sizeof(pathname), "/AMERICAN/FOLSOM/FLOW-RES IN/01JAN2006/1DAY/C:000101|RUN A/", _TRUNCATE);
	catStruct = zstructCatalogNew();
	nPathname = zcollectionCat(ifltab, pathname, catStruct);

	nPathname = strlen(pathname);
	filePos[0] = 0;
	if (zgetVersion((long long*)ifltab) == 6) {
		while (filePos[0] >= 0) {
			zcolist6_((long long*)ifltab, filePos, pathname, &nPathname, &status, strlen(pathname) - 1);
			if (status || (nPathname == 0)) {
				filePos[0] = -1;
			}
		}
	}


	zmessage2(ifltab, "mess 1", "second mess");
	hec_dss_zopen(ifltab, "C:/Users/q0hecwjc/Desktop/zz.dss");
	tss1 = zstructTsNew("//06192500/FLOW-ANNUAL PEAK/01jan1700 - 01jan2200/IR-Century/USGS/");
	status = ztsRetrieve(ifltab, tss1, 0, 0, 0);
	status = 0;
	handle = 0;
	while (!status) {
		zplist6_(ifltab, "", &handle, pathname, &len, &status, 0, sizeof(pathname));
	}
	tss1 = zstructTsNew("//DIVERSION PATTERN/PERCENT/01JAN1900/IR-YEAR/TABLE/");
	ztsRetrieve(ifltab, tss1, 0, 0, 0);

	stringCopy(pathname, sizeof(pathname), "/PROJTEST/TRY1/Flow/31MAY2012/15MINUTE/My F part/", _TRUNCATE);

	upperCase(pathname);
	pos = strstr(pathname, "MINUTE/");
	if (pos > 0) {
		path = pathname;
		pos += 3;
		jpos = (int)(pos - path);
		if (jpos > 0) {
			pathname[jpos] = '\0';
			pos += 3;
			strcat_s(pathname, sizeof(pathname), pos);
		}
	}

	pos = strstr(cpath1, "MINUTE/");
	pos += 3;
	i = pos - cpath1;
	cpath1[i] = '\0';
	pos += 3;
	strcat_s(cpath1, sizeof(cpath1), pos);

	stringCopy(cpath1, sizeof(cpath1), "/PROJTEST/TRY1/Flow/31MAY2012/15MINUTE/My F part/", _TRUNCATE);
	pos = strstr(cpath1, "MINUTE/");
	pos += 3;
	i = pos - cpath1;
	cpath1[i] = '\0';
	stringCopy(cpath2, sizeof(cpath2), cpath1, _TRUNCATE);
	pos += 4;
	strcat_s(cpath2, sizeof(cpath2), pos);

	catStruct = zstructCatalogNew();
	status = hec_dss_zopen(ifltab, "coll7.dss");
	status = zcatInternalSort(ifltab, 0, catStruct, 0, 0, 1);
	for (i = 0; i < catStruct->numberPathnames; i++) {
		printf("%s\n", catStruct->pathnameList[i]);
	}
	
	status = hec_dss_zopen(ifltab, "C:/CWMS/forecast/2017.05.02-1800/Russian_River/hms/RussianRiver.dss");
	pdsd1 = zstructPdNew("//LAKE SONOMA/ELEVATION-AREA///TABLE/");
	zpdRetrieve(ifltab, pdsd1, 0);
	zclose(ifltab);

	status = hec_dss_zopen(ifltab, "sample7.dss");
	pdsd1->xprecision = -1;
	pdsd1->yprecision = -1;
	zpdStore(ifltab, pdsd1, 0);
	zstructFree(pdsd1);
	pdsd1 = zstructPdNew("//LAKE SONOMA/ELEVATION-AREA///TABLE/");
	zpdRetrieve(ifltab, pdsd1, 0);

	recordBasics = zstructRecordBasicsNew("//SACRAMENTO/TEMP-MAX/01JAN1980/1DAY/OBS/");
	zgetRecordBasics(ifltab, recordBasics);
	//
	/*
	zopenExtended(ifltab, "sample7.dss", 7,
		3, 0, 0, 0);
	for (i = 0; i < 10; i++) {
		tss1 = zstructTsNew("//SACRAMENTO/TEMP-MAX/01JAN1980/1DAY/OBS/");
		status = ztsRetrieve(ifltab, tss1, -1, 1, 0);
		tss1->floatValues[5] = 100.0;
		tss1->pathname = "//SACRAMENTO/TEMP-MAX/01JAN1980/1DAY/OBSxx/";
		status = ztsStore(ifltab, tss1, 0);
	}

	/*status = zrecordReport(ifltab, "//SACRAMENTO/PRECIP-INC/01Jan1880/1Day/OBS/", addresses,
		&ipos, &count);
	zinquireChar(ifltab, "vers", "//SACRAMENTO/PRECIP-INC/01Jan1880/1Day/OBS/", 5, &ipos);
	//zundelete(ifltab, "//SACRAMENTO/PRECIP-INC/01Jan1880/1Day/OBS/");

	tinStructRetrieve = zstructSpatialTinNew("/a/b/c/01jan2001:1200/01jan2001:1300/f/");
	status = zspatialTinRetrieve(ifltab, tinStructRetrieve, 1);
* /

	status = hec_dss_zopen(ifltab, "C:/hmsproj7/c7.dss");
	tss1 = zstructTsNew("//SUBBASIN-1/FLOW-UNIT GRAPH/TS-PATTERN/5MIN/RUN:CURRENT/");
	// tss1->boolRetrieveAllTimes = 1;
	status = ztsRetrieve(ifltab, tss1, 0, 0, 0);
	zstructFree(tss1);
	tss1 = zstructTsNew("/CALSIM/S91/STORAGE/01Jan1940/1Month/2005A01A/");
	// tss1->boolRetrieveAllTimes = 1;
	status = ztsRetrieve(ifltab, tss1, -1, 0, 0);
	//testPairedData4(ifltab);
	zclose(ifltab);
	return 0;
	/*
	//status = hec_dss_zopen(ifltab, "M:/O'Connell/Public/for Bill/DSS no data/svSA_33.dss");
//	zset("mlvl", "", 11);
	//status = hec_dss_zopen(ifltab, "sample7.dss");
	
	unlink("pd7.dss");
	status = hec_dss_zopen(ifltab, "pd7.dss");
	testPairedData4(ifltab);
	zclose(ifltab);
	return 0;
/*	stringCopy(cpath1, sizeof(cpath1), "/FIRST FORK SINNEMAHONING CREEK/WHARTON, PA/FLOW-ANNUAL PEAK//IR-CENTURY/USGS/", _TRUNCATE);
	searchOption = 0;

	ztsends_ (ifltab, cpath1, &searchOption, startJulian,
              startMinutes, endJulian, endMinutes,
              exists,
              strlen(cpath1));

	printf("startMinutes type = %d\n", startMinutes[0]);
	return;


	/*	
	int number[1], ex[1], itype[1];
	status = hec_dss_zopen(ifltab, "febv7.dss");
	stringCopy(cpath1, sizeof(cpath1), "/PROJTEST/TRY1//31MAY2012:0700/01JUN2012:0700//", _TRUNCATE);
	zdtype_ ((long long*)ifltab, cpath1, number, ex,
             cpath2, itype,
             strlen(cpath1), sizeof(cpath2)-1);
	printf("Data type = %d\n", itype[0]);

	/*	
	int year, month, day, i, julian, seconds, julianBlockDate, julianNextBlockDate;
	char c[30];

	julian = yearMonthDayToJulian (0, 12, 31);
	julianToDate(julian, 0, c, sizeof(c));
	printf("Loc 1, julian = %d,  date = %s\n",julian, c);

	julian = yearMonthDayToJulian(0, 1, 1);
	julian += JULIAN_BASE_DATE;
	julian = yearMonthDayToJulian(-1, 12, 31);
	julian += JULIAN_BASE_DATE;
	julian = yearMonthDayToJulian(-1, 12, 1);
	julian += JULIAN_BASE_DATE;
	/*
	julianBlockDate = -2520171;
	//julianNextBlockDate = ztsIncrementBlock(julianBlockDate, 5);
	julian = -693961;
	julian = -693595;
	seconds = 86400;
	julian = 1;
	julianToDate(julian, 0, c, sizeof(c));
	printf("Loc a, julian = %d,  date = %s\n",julian, c);
	//ztsOffsetAdjustToStandard(31536000, &julian, &seconds);
	status = incrementTime(31536000, -1, julian, seconds, &julian, &seconds);
	julianToDate(julian, 0, c, sizeof(c));
	printf("Loc b, julian = %d,  date = %s\n",julian, c);

	julian = -JULIAN_BASE_DATE;
	julianToDate(julian, 0, c, sizeof(c));
	printf("Loc d, JULIAN_BASE_DATE = %d,  date = %s\n",julian, c);

	julian = -JULIAN_BASE_DATE + 1;
	julianToDate(julian, 0, c, sizeof(c));
	printf("Loc e, JULIAN_BASE_DATE + 1 = %d,  date = %s\n",julian, c);

	julian = -JULIAN_BASE_DATE - 1;
	julianToDate(julian, 0, c, sizeof(c));
	printf("Loc f, JULIAN_BASE_DATE - 1 = %d,  date = %s\n",julian, c);

	julian = -JULIAN_BASE_DATE + 80;
	for (i=0; i<500; i++) {
		julianToDate(julian, 0, c, sizeof(c));
		printf("julian = %d,  date = %s\n",julian, c);
		julian--;
	}
	return;
	
	julianBlockDate = yearMonthDayToJulian(100, 1, 1);;  //-767007;


	for (i=0; i<100; i++) {
	julianToDate(julianBlockDate, 0, outName, sizeof(outName));
	printf("julianBlockDate %s,  %d\n", outName, julianBlockDate);
	julianNextBlockDate = ztsIncrementBlock(julianBlockDate, 5);
	julianToDate(julianNextBlockDate, 0, outName, sizeof(outName));
	printf("julianNextBlockDate %s,  %d\n", outName, julianNextBlockDate);
	julianBlockDate = julianNextBlockDate;
	}

	return;


	/*

	jul = yearMonthDayToJulian (-4796, 1, 1) ;
	jul = yearMonthDayToJulian (-4797, 1, 1) ;


	jul = -2444796;
	julianToYearMonthDay(jul, &year, &month, &day);
	jul2 = yearMonthDayToJulian(-4801, 12, 31);

	jul = -2444795;
	julianToYearMonthDay(jul, &year, &month, &day);
	jul--;
	julianToYearMonthDay(jul, &year, &month, &day);
	jul--;
	julianToYearMonthDay(jul, &year, &month, &day);
	jul--;
	julianToYearMonthDay(jul, &year, &month, &day);
	jul = dateToJulian("20Jan-4000");
/*
	for (i=0; i<50000; i++) {
		julianToYearMonthDay(jul, &year, &month, &day);
		jul2 = yearMonthDayToJulian(year, month, day);
		julianToDate(jul, 0, outName, sizeof(outName));
		//printf(" %s,  %d,     %d  %d  %d\n", outName, jul, year, month, day);
		if (jul2 != jul) {
			printf ("Julian miss-match; Failure.  Original jul %d, computed jul %d,  date: %s\n", jul, jul2, outName);
		}
		jul--;
	}

	return;
	jul = dateToJulian("01Jan-4800");
	julianToYearMonthDay(jul, &year, &month, &day);
	jul = dateToJulian("05May-4800");
	julianToYearMonthDay(jul, &year, &month, &day);
	jul = dateToJulian("31Dec-4800");
	julianToYearMonthDay(jul, &year, &month, &day);


	strncpy_s(fileName7, sizeof(fileName7), "testDss7.dss", sizeof(fileName7));
	_unlink(fileName7);
	status = hec_dss_zopen(ifltab7, fileName7);
	if (status) return status;
	//zsetMessageLevel(MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1);
	status = testExpandedTimesIrreg2(ifltab7);
	if (status != STATUS_OKAY) return status;
	if (status == STATUS_OKAY) return status;

/*	testArrayWriteRead(ifltab7);
/*	status = testAlias(ifltab7);
	if (status) return status;
return status;

	status = testExpandedTimesIrreg2(ifltab7);
	if (status != STATUS_OKAY) return status;
	if (status == STATUS_OKAY) return status;
	/*
	jul = dateToJulian("20May5000000");
	julianToDate(jul, 4, fileName6, sizeof(fileName6));
	printf("-->%s<--\n", fileName6);

		jul = dateToJulian("20May-5000000");
	julianToDate(jul, 4, fileName6, sizeof(fileName6));
	printf("-->%s<--\n", fileName6);
	
	//status = testMultiUser();
	//return;
	*/
	//status = testTin(ifltab7);
	//return;


//	zopenInternal(ifltab, "SmallHash7.dss", 0, 0, 
//	 48, 0, 0);
//	zclose(ifltab);

/*
	
	status = hec_dss_zopen(ifltab, "sample7.dss");
	if (status != STATUS_OKAY) return;
	//zset("MLVL", "", 15);
	tss1 = zstructTsNew("//SACRAMENTO/TEMP-MAX/01JAN1980/1DAY/OBS/");
	//tss1->boolRetrieveAllTimes = 1;
	status = ztsRetrieve(ifltab, tss1, -1, 1, 0);

	tss1->pathname = mallocAndCopy("//SACRAMENTO/TEMP-MAX/01JAN1980/1DAY/test/");
	tss1->pathnameInternal = 0;
	tss1->timeWindow = 0;
	tss1->endJulianDate = 0;
	tss1->endTimeSeconds = 0;
	tss1->timeIntervalSeconds = 0;
	for (i = 0; i < tss1->numberValues; i++) {
		tss1->floatValues[i] = -901.;
	}
	ztsStore(ifltab, tss1, 0);

	ipos =  ztsProcessTimes(ifltab, tss1, 1);

	tss2 = zstructTsNew("/AMERICAN/FOLSOM/FLOW-RES IN//1Day/OBS/");
	tss2->times = tss1->times;
	tss2->floatValues = tss1->floatValues;
	tss2->numberValues = tss1->numberValues;
	
	ipos =  ztsProcessTimes(ifltab, tss2, 1);
	ztsMessTimeWindow(ifltab, DSS_FUNCTION_ztsRetrieve_ID, tss2);

	/*	

	strncpy_s(fileName7, sizeof(fileName7), "testTimes7.dss", sizeof(fileName7));
	_unlink(fileName7);
	status = hec_dss_zopen(ifltab7, fileName7);
	if (status) return status;


	
	//zset("MLVL", "", 12);
	status = testExpandedTimesIrreg(ifltab7);
	zclose(ifltab7);
	if (status != STATUS_OKAY) return status;
	if (status == STATUS_OKAY) return status;


	stringCopy(cpath1, sizeof(cpath1), "/Basin/Location/Flow/01Apr512346/15MINute/My f part/", _TRUNCATE);
	cpath = ztsPathCompatible(6, cpath1, strlen(cpath1));

	fileTime = 1800000 + 328444;
	fileTime *= 1440;
	status = (int) fileTime;
	status = numberPeriods(60, -328444, 1440, 1800000, 1440);

	status = hec_dss_zopen(ifltab,"wat.dss");
	tss1 = zstructTsNew("//BONNERS FERRY FLOW-LOC/FLOW/01Oct1000 - 01Oct5950/1Day/KL_STD_B:FRA_50YEAR:RESSIM-F1_SB_F/");
	status = ztsRetrieve(ifltab, tss1, 0, 1, 1);
	//BONNERS FERRY FLOW-LOC/FLOW/01Jan1000/1Day/KL_STD_B:FRA_50YEAR:RESSIM-F1_SB_F/


	/////////////////////////////////////////
	//status = hec_dss_zopen(ifltab,"TimeWIndowIntervalSim.dss");
	status = hec_dss_zopen(ifltab,"Mark.dss");
	stringCopy(cpath1, sizeof(cpath1), "B=AG RESORT C=FLOW E=15MIN F=TEST:FEB_1986:HMS-(RUN)FEB_1986", _TRUNCATE);
	//stringCopy(cpath1, sizeof(cpath1), "B=AG RESORT C=FLOW E=15Minute F=TEST:FEB_1986:HMS-(RUN)FEB_1986", _TRUNCATE);
	i = 0;
	j = 0;
	zplist7(ifltab, cpath1, &i, cpath2, &j, &status, strlen(cpath1), sizeof(cpath2));

	////////////////////////////////////////////
	

	strncpy_s(fileName7, sizeof(fileName7), "testTimes7.dss", sizeof(fileName7));
	status = hec_dss_zopen(ifltab7, fileName7);
	if (status) return status;
	catStruct = zstructCatalogNew();
	//  int zcatalog(long long *ifltab, const char *pathWithWild, zStructCatalog *catStruct, int boolSorted);
	numberPaths = zcatalog(ifltab7, (const char *)0, catStruct, 0);
	printf("%s\n", catStruct->pathnameList[0]);


	status = hec_dss_zopen(ifltab, "sample7.dss");
	if (status != STATUS_OKAY) return;
	//  Get the last write time of the file (in mills)
	fileTime = zgetLastWriteTimeFile(ifltab);
	printf("filetime = %lld\n",fileTime);
	zinquireChar(ifltab,"nrec", cpath3, sizeof(cpath3), &numberPaths);
	printf("numberPaths = %d\n",numberPaths);

	

	ztransfer = zstructTransferNew("/TEST/COWLITZ/FREQ-FLOW/EXAMPLE///", 1);
	status = zread(ifltab, ztransfer);

	count = 128;
	ipos = 0;
	for (j=0; j<6; j++) {
		cpath1[ipos++] = '/';
		for (i=0; i<20; i++) {
			cpath1[ipos++] = count++;
		}
	}
	cpath1[ipos++] = '/';
	cpath1[ipos++] = '\0';
	printf("Path ==>%s<==\n", cpath1);


	strncpy_s(fileName7, sizeof(fileName7), "testTimes7.dss", sizeof(fileName7));
	_unlink(fileName7);
	status = hec_dss_zopen(ifltab7, fileName7);
	if (status) return status;
	status = zcheckFile(ifltab7);

	//stringCopy(cpath1, sizeof(cpath1), "/TEST/COWLITZ/FREQ-FLOW/EXAMPLE//Numb 2/", _TRUNCATE);

	//zset("mlvl", "", 15);
	ztransfer->pathname = cpath1;
	status = zwrite(ifltab7, ztransfer);
	//zclose(ifltab7);

	//status = hec_dss_zopen(ifltab7, fileName7);
	if (status) return status;
	status = zcheckFile(ifltab7);

	ztransfer->allocated[zSTRUCT_pathname] = 0;
	zstructFree(ztransfer);

	ztransfer = zstructTransferNew(cpath1,1);
	//zset("mlvl", "", 15);
	status = zread(ifltab7, ztransfer);
	zclose(ifltab7);
	return -1;

	//zcheckLinks(ifltab);



	//zrecordAddresses(ifltab, "/EF RUSSIAN/COYOTE/PRECIP-INC/01Mar2006/1Hour/dup/", addresses);
	//zset("MLVL", "", 16);
	zcheckInternal(ifltab, "/RUSSIAN/NR UKIAH/FLOW/01Mar2006/1Hour/bu/", 0);
	zduplicateRecord(ifltab, "/RUSSIAN/NR UKIAH/FLOW/01Mar2006/1Hour//", "/RUSSIAN/NR UKIAH/FLOW/01Mar2006/1Hour/bu/");

	fileTime = zgetLastWriteTimeFile(ifltab);
	printf("filetime = %lld\n",fileTime);
	zinquireChar(ifltab,"nrec", cpath3, sizeof(cpath3), &numberPaths);
	printf("numberPaths = %d\n",numberPaths);




	//zsetMessageLevel(MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_2);
	status = hec_dss_zopen(ifltab, "sample6.dss");
	catStruct = zstructCatalogNew();
	status = zcatalog((long long*)ifltab, catStruct->pathWithWildChars, catStruct, 0);


	tss1 = zstructTsNew("/Basin/Location/Flow/01Apr512346/~1Hour/ITS-Large Dates/");
	status = ztsRetrieve(ifltab, tss1, 0, 1, 1);
	 zcatalogFile(ifltab, "test7.txt", 1, 0);
	
/*	strncpy_s(fileName7, sizeof(fileName7), "testTimes7.dss", sizeof(fileName7));
	_unlink(fileName7);
	status = hec_dss_zopen(ifltab7, fileName7);
	if (status) return status;
	
	status = testExpandedTimesIrreg(ifltab7);
	if (status != STATUS_OKAY) return status;
	if (status == STATUS_OKAY) return status;
/*
	unlink("ru7.dss");
	status = hec_dss_zopen(ifltab, "ru7.dss");
	if (status) return status;
	status = testTimeSeriesPattern(ifltab);
	zclose(ifltab);
	return status;

//	zconvertVersion("Pet7.dss", "Pet6.dss");

	zsetMessageLevel(zmessaging_copy_ID, MESS_INTERNAL_DIAG_2);
	//zopenDisk("small7.dss", &handle, 0, 0);
	//  zcopyRecord((long long*)ifltabFrom, (long long*)ifltabTo, pathnameFrom, pathnameTo);
/*
	hec_dss_zopen(ifltab, "FailedMerge/HMS.dss");
	hec_dss_zopen(ifltab2, "FailedMerge/toMerge/HMS.dss");
	//"/HMS/Hydrologic Sampling-Flow Sampler/Probability-Flow/Frequency/Sayers Inflow Jct - PEAK FLOW MAX/C:000025|Existing C:500 Year H:HydroSampl-Flow Sampler/"
	zcopyRecord(ifltab, ifltab2, 
		"/HMS/Hydrologic Sampling-Flow Sampler/Probability-Flow/Frequency/Sayers Inflow Jct - PEAK FLOW MAX/C:000025|Existing C:500 Year H:HydroSampl-Flow Sampler/",
		"/HMS/Hydrologic Sampling-Flow Sampler/Probability-Flow/Frequency/Sayers Inflow Jct - PEAK FLOW MAX/C:000025|Existing C:500 Year H:HydroSampl-Flow Sampler/");

	unlink( "GriddedPrecip6.dss");
	zconvertVersion("GriddedPrecip7.dss", "GriddedPrecip6.dss");




	hec_dss_zopen(ifltab, "C:/hmsproj/tifton/tifton.dss");
	tss1 = zstructTsNew("//74006/FLOW-UNIT GRAPH/TS-PATTERN/1HOUR/RUN:RUN 1/");
	status = ztsRetrieve(ifltab, tss1, 0, 0, 0);
	//status = ztsRetrievePattern(ifltab, tss1, 0);
	//zreadInfo(ifltab, "/AMERICAN/FOLSOM/PRECIP-BASIN/01Jan2006/1Day/OBS/", 0);
	//zcopyRecordInternal(ifltab, ifltab, "/AMERICAN/FOLSOM/PRECIP-BASIN/01Jan2006/1Day/New/");
	zopen(ifltab2, "ru7.dss");
	zcopyRecord(ifltab, ifltab2, "//74006/FLOW-UNIT GRAPH/TS-PATTERN/1HOUR/RUN:RUN 1/",
		"//74006/FLOW-UNIT GRAPH/TS-PATTERN/1HOUR/RUN:RUN 1/");
	tss2 = zstructTsNew("//74006/FLOW-UNIT GRAPH/TS-PATTERN/1HOUR/RUN:RUN 1/");
	status = ztsRetrieve(ifltab2, tss2, 0, 0, 0);
	zclose(ifltab);
	zclose(ifltab2);
	return 0;

	//_finite(pid);

	//zsetMessageLevel(zmessaging_put_ID, MESS_INTERNAL_DIAG_2);
	//zsetMessageLevel(zmessaging_get_ID, MESS_INTERNAL_DIAG_2);
/*
	status = hec_dss_zopen(ifltab, "new7.dss");
	testztsStruct1(ifltab);
	zclose(ifltab);
	return 0;

	status = hec_dss_zopen(ifltab, "sample7.dss");
	if (status != STATUS_OKAY) return;
	//  Get the last write time of the file (in mills)
	fileTime = zgetLastWriteTimeFile(ifltab);

	//  Do some stuff and write to the file

	catStruct = zstructCatalogNew();
	catStruct->lastWriteTime = fileTime;
	catStruct->lastWriteTimeFlag = -2;  //	-2:	time <  lastWriteTime
	//  int zcatalog(long long *ifltab, const char *pathWithWild, zStructCatalog *catStruct, int boolSorted);
	numberPaths = zcatalog(ifltab, (const char *)0, catStruct, 0);
	if (numberPaths > 0) {
		for (i=0; i<catStruct->numberPathnames; i++) {
			printf("%s\n", catStruct->pathnameList[i]);
		}
	}
	zstructFree(catStruct);



	//status = hec_dss_zopen(ifltab, "test777.dss");

	/*
	status = hec_dss_zopen(ifltab, "test.dss");
	nfound = 19;
	stringCopy(cpath1, sizeof(cpath1), "filecheck.txt", _TRUNCATE);
	fortranopen_(&nfound, cpath1, strlen(cpath1));
	zset("MUNIT", " ", 19);
	zcheckPathnames(ifltab);
	fortranclose_(&nfound);
	*/

		//int exists[1];
		
/*		int infoSize[1];
		int ndataCompressed[1];
		int dummy[1];
		int idummy[1];
		short sdummy[2];
		int plan[1];
		short *dataCompressed;
		int gridInfoFlatSize;
		int *info;

		short *dataCompresseda;
		int gridInfoFlatSizea;
		int *infoa;
		
		ztsTimeWindow timeWindowx;
		//stringCopy(cpath1, sizeof(cpath1), "/PROJTEST/TRY1/c/31MAY20011 - 01JUN20012/1Day/f/", _TRUNCATE);

		//ztsGetPathTimeWindow(7, cpath1, sizeof(cpath1), &timeWindowx); 

        // Read ------------------------------------------------------------------------
		//status = hec_dss_zopen(ifltab, "febv7.dss");
		
		stringCopy(cpath1, sizeof(cpath1), "/PROJTEST/TRY1//31MAY2012:0700/01JUN2012:0700//", _TRUNCATE);
		/* First call to zreadx gets the sizes of the flattened grid info array
		 * (stored as the internal header of the grid record) and the actual
		 * compressed data array
		 */
/*		status = hec_dss_zopen(ifltab, "C:/Users/q0hecwjc/Desktop/a7.dss");
		zcheckFile(ifltab);

		plan[0] = 1;
		zreadx_(ifltab, cpath1,
				dummy, &zero, infoSize,
				dummy, &zero, idummy,
				dummy, &zero, idummy,
				(int *)sdummy, &zero, ndataCompressed,
				plan, exists, strlen(cpath1));
		gridInfoFlatSizea = infoSize[0];
		infoa = (int *)calloc(gridInfoFlatSizea, 4);
		dataCompresseda = (short *)calloc(ndataCompressed[0], 4);

		zreadx_(ifltab, cpath1,
			infoa, &gridInfoFlatSizea, infoSize,
			dummy, &zero, idummy,
			dummy, &zero, idummy,
			(int *)dataCompresseda, ndataCompressed, ndataCompressed,
			plan, exists, strlen(cpath1));

		zclose(ifltab);


		status = hec_dss_zopen(ifltab, "sample7.dss");

		plan[0] = 1;
		zreadx_(ifltab, cpath1,
				dummy, &zero, infoSize,
				dummy, &zero, idummy,
				dummy, &zero, idummy,
				(int *)sdummy, &zero, ndataCompressed,
				plan, exists, strlen(cpath1));
		gridInfoFlatSize = infoSize[0];
		info = (int *)calloc(gridInfoFlatSize, 4);
		dataCompressed = (short *)calloc(ndataCompressed[0], 4);

		zreadx_(ifltab, cpath1,
			info, &gridInfoFlatSize, infoSize,
			dummy, &zero, idummy,
			dummy, &zero, idummy,
			(int *)dataCompressed, ndataCompressed, ndataCompressed,
			plan, exists, strlen(cpath1));

		zclose(ifltab);

		for (j=0; j<gridInfoFlatSize; j++) {
			if (infoa[j] != info[j]) {
				printf("Missmatch at %d\n", j);
			}
		}

		for (j=0; j<ndataCompressed[0]; j++) {
			if (dataCompressed[j] != dataCompresseda[j]) {
				printf("Missmatch at %d\n", j);
			}
		}


		return;


	pid = _getpid();
	printf("pid = %d\n", pid);
	hProcess = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, pid);
	dw = value;
    QueryFullProcessImageName( hProcess,0, buff, &dw);
	for (i=0; i<dw; i++) {
		cpath1[i] = buff[i];
	}
	cpath1[dw] = '\0';
	printf("EXE Path: %s\n", cpath1);


    // Print the process name and identifier.

   // _tprintf( TEXT("%s  (PID: %u)\n"), szProcessName, nfound );

    // Release the handle to the process.

    CloseHandle( hProcess );
	
	printCurrentTime(1);/*
	status = hec_dss_zopen(ifltab, "big7.dss");
	//status = hec_dss_zopen(ifltab, "sample7.dss");
	zset("MLVL", "", 1);
	QueryPerformanceFrequency(&Frequency); 
	for (i=0; i<5; i++) {
		tss1 = zstructTsNew("//SACRAMENTO/PRECIP-INC//1Day/OBS/");
		QueryPerformanceCounter(&StartingTime);
		status = ztsRetrieve(ifltab, tss1, 0, 0, 0);
		QueryPerformanceCounter(&EndingTime);
		ElapsedMicroseconds.QuadPart = EndingTime.QuadPart - StartingTime.QuadPart;
		ElapsedMicroseconds.QuadPart *= 1000000;
		ElapsedMicroseconds.QuadPart /= Frequency.QuadPart;
		longWithCommas(cbuff, sizeof(cbuff), (long long)ElapsedMicroseconds.QuadPart);
		printf("%d, time = %s   %s\n\n", i, cbuff, tss1->pathname);
		zstructFree(tss1);

		tss1 = zstructTsNew("//SACRAMENTO/TEMP-MAX/01Jan1900 - 31Dec1999/1Day/OBS/");
		QueryPerformanceCounter(&StartingTime);
		status = ztsRetrieve(ifltab, tss1, 0, 0, 0);
		QueryPerformanceCounter(&EndingTime);
		ElapsedMicroseconds.QuadPart = EndingTime.QuadPart - StartingTime.QuadPart;
		ElapsedMicroseconds.QuadPart *= 1000000;
		ElapsedMicroseconds.QuadPart /= Frequency.QuadPart;
		longWithCommas(cbuff, sizeof(cbuff), (long long)ElapsedMicroseconds.QuadPart);
		printf("%d, time = %s   %s\n\n", i, cbuff, tss1->pathname);
		zstructFree(tss1);

		tss1 = zstructTsNew("//SACRAMENTO/TEMP-MIN/01Jan1900 - 31Dec1999/1Day/OBS/");
		QueryPerformanceCounter(&StartingTime);
		status = ztsRetrieve(ifltab, tss1, 0, 0, 0);
		QueryPerformanceCounter(&EndingTime);
		ElapsedMicroseconds.QuadPart = EndingTime.QuadPart - StartingTime.QuadPart;
		ElapsedMicroseconds.QuadPart *= 1000000;
		ElapsedMicroseconds.QuadPart /= Frequency.QuadPart;
		longWithCommas(cbuff, sizeof(cbuff), (long long)ElapsedMicroseconds.QuadPart);
		printf("%d, time = %s   %s\n\n", i, cbuff, tss1->pathname);
		zstructFree(tss1);

	}
	zclose(ifltab);
	printCurrentTime(1);
	return 0;

	//  int zcatalogFile(long long *ifltab, const char *catalogFilename, int boolSorted, const char *pathWithWildChars);
	nfound = zcatalogFile(ifltab, (const char *)0, 0, (const char *)0);
	zclose(ifltab);
	printCurrentTime(1);
	return nfound;
	*/
	//zset("mlvl", "", 10);
	//_unlink("a6.dss");
	//status = zconvertVersion("sample7.dss", "a6.dss");
	//return status;
	printCurrentTime(1);
	stringCopy(cpath3, sizeof(cpath3), "//SACRAMENTO/TEMP-MIN//1Day/", _TRUNCATE);
	zset("MLVL", "", 3);
	status = hec_dss_zopen(ifltab2, "\"sample7.dss\"");


/*     The lockFlag (second) argument  is as follows:
*        0 - Unlock
*        1 - Lock for write.  
*        2 - Test to see if another process has this file locked.  (Do not lock.)
*        3 - Test for number processes have this file lock.  (Do not lock.)
*/
//	status = zlockWrite(ifltab2, 1) ;

	//printf("\n\nNote:   Times include sleeps between writes\n\n\n");

	tss1 = zstructTsNew("//SACRAMENTO/TEMP-MAX//1Day/OBS/");
	status = ztsRetrieve(ifltab2, tss1, -1, 1, 1);
	zclose(ifltab2);
	///////////////
	//zstructFree(tss1);
	//return 0;
	///////////////

//zset("MLVL", "", 15);
	if (tss1->allocated[zSTRUCT_pathname]) free(tss1->pathname);
	tss1->allocated[zSTRUCT_pathname] = 0;
	//status = zopenExtended(ifltab2, "Z:/b7.dss", 7, 0, 5000000, 0,0);
	status = zopenExtended(ifltab2, "D:/temp/nbig7.dss", 7, 0, 5000000, 0,0);
	//status = zopenExtended(ifltab2, "tbig7.dss", 7, 0, 5000000, 0,0);
	//status = zopenExtended(ifltab2, "test.dss", 7, 2, 5000000, 0,0);
	//zsetMessageLevel(9,6);
	for (i=0; i<5000000; i++) {
		getCurrentDateTime (&julian, &secondsPastMidnight, &millsPastSecond);
		secondsPastMidnight *= 1000;
		secondsPastMidnight += millsPastSecond;
		pathStr(cpath1, i);
		strncat(cpath1, &cpath3[1],sizeof(cpath1) );		
//		strcat_s (cpath1, sizeof(cpath1), &cpath3[1]);		
		_snprintf_s(cpath2, 20, 20, 	"%d/", 	secondsPastMidnight);
		//strcat_s (cpath1, sizeof(cpath1), cpath2);
		strncat (cpath1, cpath2,sizeof(cpath1));

		if (tss1->allocated[zSTRUCT_pathname]) {
			free(tss1->pathname);
		}
		tss1->pathname = strdup(cpath1);
		ztsStore(ifltab2, tss1, 0);
		j = i/100;
		j *= 100;
		if (j == i) {
			printCurrentTime(0);
			printf("    %d\n", i);
		}
		//Sleep(100);
	}

	zstructFree(tss1);
	zclose(ifltab2);
	printCurrentTime(1);
	return 0;
	/*
	_unlink("testDead.dss");
	status = hec_dss_zopen(ifltab, "testDead.dss");
	status = testTimeSeries5(ifltab);
	return status;
	zopenExtended(long long *ifltab, const char *dssFilename, int fileVersion, 
			 int access, int maxExpectedPathnames, int hashSize, int binSize)

	status = hec_dss_zopen(ifltab, "sample6.dss");
	//zset("MLVL", "", 12);
	//status = zcatalogFile((long long *)ifltab, catalogName, boolSorted, pathWithWild);
	status = zcatalogFile(ifltab, "sample6.dsc", 0, "");
	if (status != STATUS_OKAY) return status;
	catStruct = zstructCatalogNew();
	stringCopy(pathWithWild, sizeof(pathWithWild), " ", _TRUNCATE);
	status = zcatalog(ifltab, pathWithWild, catStruct, 0);
	for (i=0; i<catStruct->numberPathnames; i++) {
		printf("%s\n", catStruct->pathnameList[i]);
	}
	return status;

/*	status = hec_dss_zopen(ifltab2, "tt.dss");
	if (status != STATUS_OKAY) return status;
	status =  zcopyFile(ifltab, ifltab2, 0);


	//status = hec_dss_zopen(ifltab, "SSP/SSP2.1DSS7_Testing/20160226_testing/20160226_SSPwDSS7_testing/20160226_SSPwDSS7_testing.dss");
	status = hec_dss_zopen(ifltab, "C:/Weather/weather.dss");
	if (status != STATUS_OKAY) return status;
	*/

	/*
	status = hec_dss_zopen(ifltab, "detune.dss");
	tss1 = zstructTsNewTimes("/a/b/c/01Jan1900/1Day/f/", "01jan1900", "2400", "31dec1999", "2400");
	status = ztsRetrieve((long long*)ifltab, tss1, 0, 0, 0);
	zclose(ifltab);

	status = hec_dss_zopen(ifltab, "w1.dss");
	zsetFile(ifltab, "detune" , "", 1);
	ztsStore(ifltab, tss1, 0);
	zclose(ifltab);

	

	//zsqueeze("z:");
	zset("mlvl", "", 10);
	//status = hec_dss_zopen(ifltab2, "sample7.dss");
	status = hec_dss_zopen(ifltab2, "DssFiles/simulation7_fromWaterhsed6.dss");
	zsqueezeNeeded(ifltab2);
	//status = hec_dss_zopen(ifltab, "s7.dss");
	//status = hec_dss_zopen(ifltab2, "s7c.dss");
	//status = zcopyFile(ifltab, ifltab2, 0);
	zsqueeze7(ifltab, 1, 1);
	//Sleep(300000);
	zclose(ifltab);
	//zclose(ifltab2);
	return;
	cpath = mallocAndCopy("/B/SACRAMENTO/TEMP-MIN/01Jan2009/1Day/OBS/");
	zcheck(ifltab, cpath);
	/*
	searchOption = 4;
	ztsends_(ifltab, cpath, &searchOption, startJulian, startMinutes, endJulian, endMinutes, &nfound, strlen(cpath));

	//ztsrange_(ifltab, cpath, &searchOption, cpath1, cpath2, &nfound, strlen(cpath), sizeof(cpath1), sizeof(cpath2));
	return;
		
	zsetMessageLevel(zmessaging_locking_ID, MESS_INTERNAL_DIAG_2);
	/*zsetMessageLevel(zmessaging_perm_ID, MESS_INTERNAL_DIAG_2);
	zsetMessageLevel(zmessaging_get_ID, MESS_INTERNAL_DIAG_2);
	zsetMessageLevel(zmessaging_put_ID, MESS_INTERNAL_DIAG_2);  
	status = hec_dss_zopen(ifltab, "TestingPM.dss");
	pdsd1 = zstructPdNew("/TestingPM/Hydrologic Sampling-HS - St. Paul Levee 30years/Event-Flow/Output/Upstream Boundary Conditions - MAX FLOW/C:000000|Without Pr:TimeWindow:HydroSampl-HS - St. Paul Levee 30/");
	//zset("MLVL", "", 12);
	status = zpdRetrieve((long long*)ifltab, pdsd1, 2);
/*	zstructFree(pdsd1);
	zclose(ifltab);
	return status;

	
	status = hec_dss_zopen(ifltab, "w1.dss");
	tss1 = zstructTsNewTimes("/a/b/c/01Jan1900/1Day/f/", "01jan1900", "2400", "31dec1999", "2400");
	status = ztsRetrieve((long long*)ifltab, tss1, 0, 0, 0);
	return status;

	zstructFree(tss1);
	tss1 = zstructTsNewTimes("/a/b/c/01Jan1900/1Day/f/", "01jan1990", "2400", "31dec2004", "2400");
	status = ztsRetrieve((long long*)ifltab, tss1, 0, 0, 0);

	return -1;
	cpath = mallocAndCopy("//Eagleton/Wind Speed-Max/01JAN2016/1Day/Observed/");
	buffer1[0] = (long long)cpath;
	printf("String = %s\n", (char *)buffer1[0]);



	//tss1 = zstructTsNew(cpath);
	tss1 = zstructTsNewTimes(cpath, "31Dec2015", "1200", "01Jan2017", "1200");
	zset7("mlvl", "", 15);
	status = ztsRetrieve((long long*)ifltab, tss1, 0, 2, 1);
	if (status < 0) {
		return status;
	}
	return status;
*/	
	cpath = mallocAndCopy("/WEST FORK RIVER/NY/FLOW-stage///ESTIMATED/");
	zdelete(ifltab, cpath);
	//pdsd1 = zstructPdNew(cpath);
	pdsd1->pathname = cpath;
	pdsd1->boolIndependentIsXaxis = 1;
	status = zpdStore(ifltab, pdsd1, 0);
	zclose(ifltab);
	return 0;
/*
	//cpath = mallocAndCopy("/WEST FORK RIVER/NY/FLOW//1Day/ESTIMATED/");
	//cpath = mallocAndCopy("/WEST FORK RIVER/NY/FLOW/01Jan1950/1Day/ESTIMATED/");
	//cpath = mallocAndCopy("/RED RIVER/FARGO/FLOW-NAT-PERAVG/01Sep1901 - 12Aug2009/IR-Century/Vol-Dur_BC_60-day Max/");
	//cpath = mallocAndCopy("/WEST FORK RIVER/NY/FLOW//1Day/ESTIMATED/");
	cpath = mallocAndCopy("/WEST FORK RIVER/NY/FLOW-hi//r/ESTIMATED/");
	pdsd1->pathname = cpath;
	pdsd1->boolIndependentIsXaxis = 1;
	status = zpdStore(ifltab, pdsd1, 0);

	/*tss1 = zstructTsNew(cpath);
	ztsProcessTimes(ifltab, tss1, 0);
	ztsGetPathTimeWindow(7, cpath, strlen(cpath), &timeWindow) ; */

	status = hec_dss_zopen(ifltab, "Sample6.dss");
	cpath = mallocAndCopy("//SACRAMENTO/PRECIP-INC//1Day/OBS/");
	//status = ztsGetDateRange(ifltab, cpath, 1, startJulian, endJulian);
	if (status < 0) {
		return status;
	}
	

	searchOption = 5;
	ztsends_ ((long long*)ifltab, cpath, &searchOption, startJulian,
              startMinutes, endJulian, endMinutes,
              exists,
              strlen(cpath));
	
	
	
	//tss1 = zstructTsNew(cpath);
	//ztsProcessTimes(ifltab, tss1, 0);
	//ztsMessTimeWindow(ifltab, 0, tss1);
	//tss1->boolRetrieveAllTimes = 1;
	status = ztsRetrieve((long long*)ifltab, tss1, 0, 2, 1);
	if (status < 0) {
		return status;
	}


	
	cpath = mallocAndCopy("/COLUMBIA RIVER/THE DALLES, OR/FLOW-ANNUAL PEAK/01JAN1800 - 01JAN2000/IR-CENTURY/USGS/");
	tss1 = zstructTsNew(cpath);
	ztsProcessTimes(ifltab, tss1, 0);
	ztsMessTimeWindow(ifltab, 0, tss1);
	//tss1->boolRetrieveAllTimes = 1;
		status = ztsRetrieve((long long*)ifltab, tss1, 0, 2, 1);
		if (status < 0) {
			return status;
		}
	ztsMessTimeWindow(ifltab, 0, tss1);
	searchOption = 5;
	ztsends_ ((long long*)ifltab, cpath, &searchOption, startJulian,
              startMinutes, endJulian, endMinutes,
              exists,
              strlen(cpath));
printf("Pathname: %s\n", cpath);
printf("Start Julian: %d\nEnd Julian:   %d\n",startJulian[0], endJulian[0]);

zinquireChar(ifltab, "type", cbuff, sizeof(cbuff), &searchOption);
free(cpath);
zclose(ifltab);
return -1;
	status = hec_dss_zopen(ifltab, "sample.dss");
	if (status != STATUS_OKAY) return status;


	cpath = mallocAndCopy("/NE ANACOSTIA/RIVERDALE/FREQ-FLOW/MAX ANALYTICAL//1969-01 H33(MAX)/");
	pds = zstructPdNew(cpath);
	status = zpdRetrieve(ifltab, pds, 2);



	tss1 = zstructTsNew(cpath);
	//tss1->boolRetrieveAllTimes = 1;
		status = ztsRetrieve((long long*)ifltab, tss1, 0, 1, 1);
		if (status < 0) {
			return status;
		}

/*	millis = zgetLastWriteTime (ifltab, cpath);
	millis = zgetLastWriteTimeFile(ifltab);
*/

	status = hec_dss_zopen(ifltab2, "DSS-7_Testing/SSP2.1DSS7_Testing/SSP2.1wDSS7_LogFile/SSP2.1wDSS7_LogFile.dss");
	if (status != STATUS_OKAY) return status;
	status = hec_dss_zopen(ifltab, "C:/Users/q0hecwjc/Desktop/db7.dss");
	if (status != STATUS_OKAY) return status;



	//cpath = mallocAndCopy("/COLUMBIA RIVER/THE DALLES, OR/FLOW-ANNUAL PEAK/01JAN1800 - 01JAN2000/IR-CENTURY/USGS/");
	//zgetinfo_(ifltab, cpath, ibuff, &status, strlen(cpath));

	cpath = mallocAndCopy("/AMERICAN R/FAIR OAKS CA/FLOW-ANNUAL PEAK//IR-CENTURY/USGS/");
	ztsends_ ((long long*)ifltab, cpath, &searchOption, startJulian,
              startMinutes, endJulian, endMinutes,
              exists,
              strlen(cpath));


	tss1 = zstructTsNew(cpath);

		status = ztsRetrieve((long long*)ifltab, tss1, 0, 1, 1);
		if (status < 0) {
			return status;
		}

	julian = dateToJulian("10Jan1862");
	mins = julian * 1440 + 720;
	for (i=0; i<200; i++) {
		dvalues[i] = (double)i;
		itimes[i] = mins +(i * 60);
	}

	//tss1 = zstructTsNewIrregDoubles("/COLUMBIA RIVER/THE DALLES, OR/FLOW-ANNUAL PEAK/01JAN1800/IR-CENTURY/test/", dvalues, 1, itimes, MINUTE_GRANULARITY, cnull, "cfs", "Inst-Val");
	//status = ztsStore(ifltab, tss1, 0);
	//if (zcheckStatus(ifltab, status, 1, "Fail in adhoc Loc 1, store status ")) return status;

	tss2 = zstructTsNew("/COLUMBIA RIVER/THE DALLES, OR/FLOW-ANNUAL PEAK/01JAN1800/IR-CENTURY/USGS/");
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);
	if (status != STATUS_OKAY) return status;



	for (i=0; i<2; i++) {
		dvalues[i] = (float)(i + 123);
		itimes[i] = i * 1440 * 365;
	}
	tss2 = zstructTsNewIrregFloats("/LAKE HAVASU/PARKER DAM, AZ-CA/FLOW-ANNUAL PEAK/01Jan1900/IR-Century/TEST/", fvalues, MINUTE_GRANULARITY, 
		itimes, MINUTE_GRANULARITY, "20April1943", "cfs", "Inst-Val");

	status = ztsStore(ifltab, tss2, 0);

	tss1 = zstructTsNew("/LAKE HAVASU/PARKER DAM, AZ-CA/FLOW-ANNUAL PEAK/01Jan1900/IR-Century/TEST/"); 
	status = ztsRetrieve(ifltab, tss1, -1, 1, 0);
	if (status != STATUS_OKAY) return status; 


	zclose(ifltab);
	
	return status;
}

void tstCnotes() {

	long long ifltab7[250];
	char fileName7[80];
	char cnotes[10000];
	int status;
	int  j, ipos;
	float values[200];
	zStructTimeSeries  *tss2;
	char alpha[] = { "abcdefghijklmnopqrstuvwxyz" };

	stringCopy(fileName7, sizeof(fileName7), "C:/Temp/char7.dss", sizeof(fileName7));
	status = hec_dss_zopen(ifltab7, fileName7);

	/*
	zStructTimeSeries* zstructTsNewRegDoubles(const char* pathname, double *doubleValues, int numberValues,
	const char *startDate, const char *startTime,
	const char *units, const char *type)
	*/

	//  Use "quality7[][]" for multiple int quality flags.
	//  Cannot use both quality and quality7, as they both occupy the same space on disk. 
	//int quality7[][] = new int[200][4];
	//String cnotes[] = new String[200];
	ipos = 0;
	for (int i = 0; i < 200; i++) {
		values[i] = (float)i;
		int n = i / 25;
		n = i - (n * 25) + 1;
		for (j = 0; j < n; j++) {
			cnotes[ipos++] = alpha[j];
		}
		cnotes[ipos++] = '\0';
		//cnotes[i] = alpha.substring(0, n);
	//	for (int j = 0; j<4; j++) {
	//		quality7[i][j] = (i * 10) + j;
	//	}
	}
	zset("mlvl", "", 14);
	tss2 = zstructTsNewRegFloats("/Basin/Location/Flow//1Hour/Quality and Notes/", values, 200, "20Jan2010", "2400", "cfs", "inst-val");
	tss2->cnotes = cnotes;
	tss2->cnotesLengthTotal = ipos;
	status = ztsStore(ifltab7, tss2, 1);

}