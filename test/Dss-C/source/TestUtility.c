#ifdef _MSC_VER
#include <windows.h>
#else
#include <unistd.h>
#endif
#include "heclib.h"
#include "TestDssC.h"
#include "zdssKeys.h"
#include "zdssLocking.h"


int CheckPathnames(char* dssFileName)
{
	long long ifltab[250];
	int status = zopen(ifltab, dssFileName);
	if (status < 0) {
		printf("Error opening file.  status = %d\n", status);
		return status;
	}

	if (zgetVersion(ifltab) == 6) {
		zcklnk6_(ifltab, &status);
	}
	else {
		status = zcheckPathnames(ifltab);
	}
	zclose(ifltab);
	return status;
}



int CheckFile(char* dssFileName)
{
	long long ifltab[250];
	int status = zopen(ifltab, dssFileName);
	if (status < 0) {
		printf("Error opening file.  status = %d\n", status);
		return status;
	}

	status = zcheckFile(ifltab);
	zclose(ifltab);
	return status;
}

int CheckLinks(char* dssFileName)
{
	long long ifltab[250];
	int status = zopen(ifltab, dssFileName);
	if (status < 0) {
		printf("Error opening file.  status = %d\n", status);
		return status;
	}

	if (zgetVersion(ifltab) == 6) {
		zcklnk6_(ifltab, &status);
	}
	else {
		status = zcheckLinks(ifltab);
	}
	zclose(ifltab);
	return status;
}


int PrintCatalog(char* dssFileName){

	long long ifltab[250];
	int status = zopen(ifltab, dssFileName);
	if (status != 0) {
		printf("Error during open.  status= %d\n", status);
		return status;
	}
	zStructCatalog* catStruct = zstructCatalogNew();
	status = zcatalog(ifltab, (const char*)0, catStruct, 1);
	if (status < 0) {
		printf("Error during catalog.  Error code %d\n", status);
		return status;
	}
	  for (int i = 0; i <catStruct->numberPathnames; i++)
  	  {
			printf("[%d] %s\n", i,catStruct->pathnameList[i]);
 	  }
			
zstructFree(catStruct);
	zclose(ifltab);
	return status;

}
int Zqueeze(char* dssFileName) {
	long long ifltab[250];
	int status = zopen(ifltab, dssFileName);
	if (status != 0) {
		printf("Error during open.  status= %d\n", status);
		return status;
	}
	return zclose(ifltab);
}

/// <summary>
/// Read any TimeSeries data in the dss file
/// </summary>
/// <param name="dssFileName"></param>
/// <returns></returns>
int ReadTimeSeries(char* dssFileName)
{
	long long ifltab[250];
	int status = zopen(ifltab, dssFileName);
	zStructCatalog* catStruct = zstructCatalogNew();
	status = zcatalog(ifltab, (const char*)0, catStruct, 1);
	zStructTimeSeries* tss;
	if (status < 0) {
		printf("Error during catalog.  Error code %d\n", status);
		return status;
	}
	for (size_t i = 0; i <catStruct->numberPathnames; i++)
	{
		int recType = zdataType(ifltab, catStruct->pathnameList[i]);

		if (recType >= 100 && recType < 200) // see zdataTypeDescriptions.h
		{
			tss = zstructTsNew(catStruct->pathnameList[i]);
			status = ztsRetrieve(ifltab, tss, -1, 1, 0);
			//printf("tss->numberValues = %d\n", tss->numberValues);
			zstructFree(tss);
			if (status != STATUS_OKAY) break;
		}
			
	}
	printf("read %d pathnames\n",catStruct->numberPathnames);
	zstructFree(catStruct);
	zclose(ifltab);
	return status;

}
int WriteTimeSeries2(long long ifltab[250], int timeSeriesCount, int timeSeriesLength);

int WriteTimeSeries(char* dssFileName, int version, int timeSeriesCount, int timeSeriesLength)
{
	long long ifltab[250];
	int status;
	if (version == 6)
		status = zopen6(ifltab, dssFileName);
	else
		status = zopen(ifltab, dssFileName);

	if (status != STATUS_OKAY) return status;

	status = WriteTimeSeries2(ifltab, timeSeriesCount, timeSeriesLength);
	zclose(ifltab);
	return status;

}
int WriteTimeSeries2(long long ifltab[250],int timeSeriesCount, int timeSeriesLength){
	float* fvalues = 0;
	zStructTimeSeries* tss1 = 0;
	char pathname[150];
	int i, status=0;
	for (size_t series = 0; series < timeSeriesCount; series++)
	{
		fvalues = malloc(timeSeriesLength * sizeof(float));
		for (i = 0; i < timeSeriesLength; i++) {
			fvalues[i] = (float)i;
		}
		sprintf(pathname, "/testing/timeseries/Flow//1Day/series%d/", (int)series);
		//tss1 = zstructTsNewRegFloats(pathname, fvalues, timeSeriesLength, "01Jan1900", "1200", "cfs", "Inst-Val");
		tss1 = zstructTsNewRegFloats(pathname, fvalues, timeSeriesLength, "01Jan1900", "1200", "cfs", "Inst-Val");
		status = ztsStore(ifltab, tss1, 0);
		zstructFree(tss1);
		if (status != STATUS_OKAY)
			break;
	}
	
	return status; 
}

int Workout(char* exe, char* version, char* timeSeriesCount,char* timeSeriesLength, char* dssFileName) {
	int status = 0;
	int ver = atoi(version);
	int count = atoi(timeSeriesCount);
	int length = atoi(timeSeriesLength);
	//zsetMessageLevel(MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1);
	if (!(ver == 6 || ver == 7))
	{
		usage(exe);
		return -1;
	}
	char* fn = dssFileName;
	printf("\nstarting workout with file: %s", fn);
	status = WriteTimeSeries(fn, ver, count, length);
	if (status != 0)
		return status;
	status = ReadTimeSeries(fn);
	return status;
}



void PrintTable(long long ifltab[250]) {
	int* ifltab6 =(int*) &ifltab;

	printf("V7 klockLevel: %lld", ifltab[zdssKeys.klockLevel]);
	printf("V6 klockLevel: %d", ifltab6[zdssKeys.klockLevel]);// to DO what index for v6

	for (int i = 0; i < 250; i++)
	{
		long long x7 = ifltab[i];
		int x6 = ifltab6[i];
		if( x7 != 0)
		  printf("ifltab[%d] :  %lld  %d %d\n",i, x7,(int)x7, x6);
	}
  }


void lockdss_(int* ihandle, int* mode, int* position, int* nbytes, int* istat);

/* lock dss file  */
int Lock(char* dssFileName, int sleepSeconds) {
	long long ifltab[250];
	for (size_t i = 0; i < 250; i++)
	{
		ifltab[i] = 0;
	}
	int status = zopen(ifltab, dssFileName);

	printf("\nattempting to lock file %s", dssFileName);

	if (zgetVersion(ifltab) == 6) {
		// dss v6
		int handle = (int)zinquire(ifltab, "UNIT");
		int mode = 2; // lock don't wait.
		int position = 8704;
		int nbytes = 4;
		lockdss_(&handle, &mode, &position,&nbytes , &status); // during write
		
		//lockdss_(int* ihandle(3), int* mode(2), int* position(8704), int* nbytes(4), int* istat) // during write
		//lockdss_(int* ihandle(3),  mode(2), int* position(8704), int* nbytes(4), int* istat) // during write
		//_spawn/ fork , threads
		//#!bash
		// ./Dss-C  lock file1.dss &
		//  rval = timeout ./Dss-C 
		// rval tells if we could write (that is bad if we can)
		//status = WriteTimeSeries2(ifltab, 11, 1234567);
	}
	else
	{ // dss v7
		status = zlockActive(ifltab, LOCKING_LEVEL_HIGH, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
	}
	printf("\nstatus = %d", status);
	if (status != 0)
	{
		printf("\nFailed to lock file %s", dssFileName);
		return status;
	}

		printf("\nSleeping for %d seconds ...",sleepSeconds);
#ifdef _MSC_VER
		Sleep(sleepSeconds*1000);
#else
		usleep((sleepSeconds * 1000*1000));
#endif

		zclose(ifltab);
		return 0;

}

void testLock(long long* ifltab, int ihandle);

int CheckLocking(char* dssFileName) {
	long long ifltab[250];
	for (size_t i = 0; i < 250; i++)
	{
		ifltab[i] = 0;
	}
	int status;
	printf("\ncheck-lock\n");
	

	status = zopen(ifltab, dssFileName);
	PrintTable(ifltab);

	if (zgetVersion(ifltab) == 6) {
		// dss v6
		int handle = (int)zinquire(ifltab, "UNIT");
		int mode = 3; // check lock status
		int position = 8704;
		int nbytes = 4;
		lockdss_(&handle, &mode, &position, &nbytes, &status); 
		printf("\nlock status = %d", status);
	/*	printf("\nifltab[KMULT]= %ld", ifltab[26]);
		printf("\nifltab[KREADO]= %ld", ifltab[35]);
			int* ifltab6 = ifltab;
			printf("\nifltab6[KMULT]= %d", ifltab6[26]);
			printf("\nifltab6[KREADO]= %d", ifltab6[35]);
			*/
			/*
Kmult=          26
 KREADO=          35
 IFLTAB(KMULT)           3
 IFLTAB(KREADO)           0
			*/
	}
	else
	{// dss 7

		int lockFlagTest = 3;
		int readAccess = 0;
		int writeAccess = 1;
		//zcheckMultiUser()
		status = zlockPassive(ifltab, lockFlagTest, readAccess);
		printf("\nreadAccess = %d\n", status);
		status = zlockPassive(ifltab, lockFlagTest, writeAccess);
		printf("\nwriteAccess = %d\n", status);
		int handle = (int)ifltab[zdssKeys.khandle];
		testLock(ifltab, handle);
	}
	zclose(ifltab);
	return status;
}

int Export(char* dssFileName, char* path)
{
	long long ifltab[250];
	char cdate[13], ctime[10];
	int status = zopen(ifltab, dssFileName);
	zStructRecordSize* rs = zstructRecordSizeNew(path);

	status = zgetRecordSize(ifltab, rs);
	if (status != 0)
	{
		printf("\n Error:  Could not find path:' %s'",path);
	}

	printf("\ndataType = %d\n",rs->dataType);

	if ((rs->dataType >= DATA_TYPE_RTS) && (rs->dataType < DATA_TYPE_PD))
	{ // time series
		zStructTimeSeries* tss = zstructTsNew(path);
		status = ztsRetrieve(ifltab, tss, 0, 2, 0);
		if (status != STATUS_OKAY) return status;

		for (int i = 0; i < tss->numberValues; i++) {
			getDateAndTime(tss->times[i], tss->timeGranularitySeconds, tss->julianBaseDate,
				cdate, sizeof(cdate), ctime, sizeof(ctime));
			printf("%s %s, %f\n", cdate, ctime, tss->doubleValues[i]);
		}
	}
	if ((rs->dataType >= DATA_TYPE_UGT) && (rs->dataType < DATA_TYPE_SG))
	{
		zStructSpatialGrid* gridStruct = zstructSpatialGridNew(path);
		(ifltab, gridStruct,1);

		int status = zspatialGridRetrieve(ifltab, gridStruct, 1);
		
		printGridStruct(ifltab, -1, gridStruct);
		if (gridStruct->_data)
		{
			float* x = (float*)gridStruct->_data;
			size_t size = gridStruct->_numberOfCellsX * gridStruct->_numberOfCellsY;
			printf("---begin GRID DATA---\n");
			for (size_t i = 0; i < size; i++)
			{
				printf("%f ", x[i]);
			}
			printf("\n--- end GRID DATA---\n");
			zstructFree(gridStruct);
		}
	}

	zstructFree(rs);
	return 0;
}