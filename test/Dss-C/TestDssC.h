#ifndef TEST_DSS_H
#define TEST_DSS_H


#include "heclib.h"
#include "hecdss7.h"

int testLargeCopy();
int test_normalize_f_part();
int test_vertical_datums_c();
int test_vertical_datums_f_();
int test_stringCat();
int testAdHoc();
int testMultiUser();
int testZtsProcessTimes(long long *ifltab);
int testztsStruct1(long long *ifltab);
int testztsStruct2(long long *ifltab);
int testztsStruct3(long long *ifltab);
int testztsStruct4(long long *ifltab);
int testztsStruct5(long long *ifltab);
int testztsStruct11(long long *ifltab);
int testztsStruct12(long long *ifltab);
int testztsStruct13(long long *ifltab);
int testztsStruct14(long long *ifltab);
int testztsStruct15(long long *ifltab);
int testTimeSeries5(long long *ifltab);
int testTimeSeriesPattern(long long *ifltab);
int testPairedData(long long *ifltab);
int TestPairedData2(long long *ifltab);
int TestPairedData3(long long *ifltab);
int testConversion();
int testTextTable(long long *ifltab);
int testWriteRead(long long *ifltab);
int testArrayWriteRead(long long *ifltab);
int testInternalIO(long long *ifltab);
int testBufferedIO(long long *ifltab);
int testlocation(long long *ifltab);
int testProfileIrreg(long long *ifltab);
int testProfileReg(long long *ifltab);
int testSqueeze(const char *filename);
int testTin(long long *ifltab);
int testExpandedTimes(long long *ifltab);
int testExpandedTimesIrreg(long long *ifltab);
int testExpandedTimesIrreg2(long long *ifltab);
int testAlias(long long *ifltab);
int testReclaim(const char *dssFilename);
int testRecordInfo6(const char *filename);
int testRecordInfo7(const char* filename);
int test_jira_dss_163_weekly_time_series_fails();



void checknumbers_(int* numb1, int* numb2, const char *mess, int *status, size_t strlenmess);
void checkfloats_(float *floatValues1, float *floatValues2, int *numberValues, 
				   const char *mess, int *status, size_t strlenmess);
void checkints_(int *intValues1, int *intValues2, int *len, int *numberValues, 
				   const char *mess, int *status, size_t strlenmess);
void checktimes_(int *times1, int *times2, int *baseDate, int *timeGranularitySeconds, int *numberValues, 
				   const char *mess, int *status, size_t strlenmess);
void checkdoubles_(double *doubleValues1, double *doubleValues2, int *numberValues, 
				   const char *mess, int *status, size_t strlenmess);

void checkstring_(const char *units1, const char *units2, const char *mess, int *status,
				  size_t strlenunits1, size_t strlenunits2, size_t strlenmess);

int compareZtwItem(int one, int two, const char *name, char *path, char *mess);
void clearztw(ztsTimeWindow *timeWindow);
int compareZtw(ztsTimeWindow *timeWindow1, ztsTimeWindow *timeWindow2, char *path, char *mess);
int compareProfiles (long long *ifltab, zStructTimeSeries* tss1, zStructTimeSeries* tss2, const char *str);

void copyTwoStrings(char *mess, int sizeofMess, const char* str1, const char* str2);
int compareTss (long long *ifltab, zStructTimeSeries* tss1, zStructTimeSeries* tss2, const char *str);
int comparePDs (long long *ifltab, zStructPairedData *pds1, zStructPairedData *pds2, const char *str);
int testMisc();
int miscTests();
int testDateTime();
int testDelete(const char *dssFilename7, const char *dssFilename6);
int runTests(long long* ifltab);
int testCatalog();
int testIO_Interface(long long *ifltab7, long long *ifltab6);
int inctim2_(int *interval, int *nperiods, int *juls, int *issecs, int *jule, int *iesecs);
int testDateFunctions();
int Bulletin_17C_SSP_Issue();
int writeDoubleArray();
int multipleWriteDeleteSlowDown();
int SolarisTesting();
int UnitPaddingIssue();
void decodeError(int errorCode);
int Lock(char* dssFileName, int sleepSeconds);
int CheckLocking(char* dssFileName);
int Workout(char* exe, char* version, char* timeSeriesCount,char* timeSeriesLength, char* dssFileName);
int CheckLinks(char* dssFileName);
int CheckPathnames(char* dssFileName);
int PrintCatalog(char* dssFileName,int printRecordType);
int Zqueeze(char* dssFileName);
int CheckFile(char* dssFileName);
void usage(char* exeName);
int Export(char* dssFileName, char* path, int metaDataOnly);
int PathnameTesting(char* dssFileName, int dssVersion);
int PrintHashTable(const char* dssFilename);
int ImportProfile(const char* csvFilename, const char* dssFilename, const char* path, const char* date,
	const char* time, const char* units, const char* datatype);
int read_profile_from_csv(zStructTimeSeries* tss, const char* csvFilename);
int units_issue_126();

#endif
