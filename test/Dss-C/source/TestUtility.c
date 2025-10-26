#ifdef _MSC_VER
#include <windows.h>
#else
#include <unistd.h>
#endif
#include "heclib.h"
#include "TestDssC.h"
#include "zdssKeys.h"
#include "zdssLocking.h"
#include <string.h>


#ifdef _MSC_VER
#define strtok_r strtok_s
#define strdup _strdup
#endif

int CheckPathnames(char* dssFileName)
{
	long long ifltab[250];
	int status = hec_dss_zopen(ifltab, dssFileName);
	if (status < 0) {
		printf("Error opening file.  status = %d\n", status);
		return status;
	}

	
	status = zcheckPathnames(ifltab);
	
	zclose(ifltab);
	return status;
}

int PrintHashTable(const char* dssFilename) {
	long long ifltab[250];
	long long tableHash=-1;
	long long binAddress;
	int status = hec_dss_zopen(ifltab, dssFilename);
	if (status < 0) {
		printf("Error opening file.  status = %d\n", status);
		return status;
	}
	long long* fileHeader = (long long*)ifltab[zdssKeys.kfileHeader];
	printf("\n Hash Table:  fileHeader[zdssFileKeys.kmaxHash] = %lld", (long long)fileHeader[zdssFileKeys.kmaxHash]);
	printf("\n hash    bin-address");
	printf("\n-------------------");

	while (1) {
		//  Need to read next hash from the hash table
		// and then pathname bin
		tableHash++;
		if (tableHash == fileHeader[zdssFileKeys.kmaxHash]) {
			//  All done - no more pathnames in file
			break;
		}

		ifltab[zdssKeys.kaddTableHash] = tableHash + fileHeader[zdssFileKeys.kaddHashTableStart];
		status = zget(ifltab, ifltab[zdssKeys.kaddTableHash], (int*)&binAddress, 1, 2);
		printf("\n %5lld %lld",tableHash, binAddress);
	}

	return 0;
}

int CheckFile(char* dssFileName)
{
	long long ifltab[250];
	int status = hec_dss_zopen(ifltab, dssFileName);
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
	int status = hec_dss_zopen(ifltab, dssFileName);
	if (status < 0) {
		printf("Error opening file.  status = %d\n", status);
		return status;
	}

	status = zcheckLinks(ifltab);
	
	zclose(ifltab);
	return status;
}


int PrintCatalog(char* dssFileName, int details){

	long long ifltab[250];
	int status = hec_dss_zopen(ifltab, dssFileName);
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
		  if (details) {
			  zStructRecordBasics* recordBasics = zstructRecordBasicsNew(catStruct->pathnameList[i]);

			// zset("MLEV", "",17 );
			  status = zgetRecordBasics(ifltab, recordBasics);
			  
				/*  zdtype_((long long*)ifltab, path, number, exists,
					  ctype, itype,
					  strlen(path), sizeof(ctype) - 1);*/

			  printf("[%d] \"%s\" %d\n", i, catStruct->pathnameList[i],recordBasics->recordType);
			  zstructFree(recordBasics);
		  }
		  else {
			  printf("[%d] %s\n", i, catStruct->pathnameList[i]);
		  }
 	  }
			
zstructFree(catStruct);
	zclose(ifltab);
	return status;

}
int Zqueeze(char* dssFileName) {
	long long ifltab[250];
	int status = hec_dss_zopen(ifltab, dssFileName);
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
	int status = hec_dss_zopen(ifltab, dssFileName);
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

int WriteTimeSeries(char* dssFileName,int timeSeriesCount, int timeSeriesLength)
{
	long long ifltab[250];
	int	status = hec_dss_zopen(ifltab, dssFileName);

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
		free(fvalues);
		if (status != STATUS_OKAY)
			break;
	}
	
	return status; 
}

int Workout(char* exe, char* timeSeriesCount,char* timeSeriesLength, char* dssFileName) {
	int status = 0;
	int count = atoi(timeSeriesCount);
	int length = atoi(timeSeriesLength);
	
	char* fn = dssFileName;
	printf("\nstarting workout with file: %s", fn);
	status = WriteTimeSeries(fn, count, length);
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
	int status = hec_dss_zopen(ifltab, dssFileName);

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
	

	status = hec_dss_zopen(ifltab, dssFileName);
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

int Export(char* dssFileName, char* path, int metaDataOnly)
{
	long long ifltab[250];
	char cdate[13], ctime[10];
	int status = hec_dss_zopen(ifltab, dssFileName);
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

		for (int i = 0; i < tss->numberValues && !metaDataOnly; i++) {
			getDateAndTime(tss->times[i], tss->timeGranularitySeconds, tss->julianBaseDate,
				cdate, sizeof(cdate), ctime, sizeof(ctime));
			printf("%s %s, %f\n", cdate, ctime, tss->doubleValues[i]);
		}
	}
	if ((rs->dataType >= DATA_TYPE_UGT) && (rs->dataType < DATA_TYPE_SG))
	{
		zStructSpatialGrid* gridStruct = zstructSpatialGridNew(path);
		

		int status = zspatialGridRetrieve(ifltab, gridStruct, !metaDataOnly);
		
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
	zclose(ifltab);
	return 0;
}

int ReadGrids(const char* file1){
	long long start_time = getCurrentTimeMillis();
	long long ifltab1[250];
	int status = hec_dss_zopen(ifltab1, file1);

	zStructCatalog* catStruct = zstructCatalogNew();
	status = zcatalog(ifltab1, (const char*)0, catStruct, 1);
	if (status < 0) {
		printf("Error during catalog.  Error code %d\n", status);
		return status;
	}
	for (int i = 0; i < catStruct->numberPathnames; i++)
	{
		zStructRecordBasics* recordBasics = zstructRecordBasicsNew(catStruct->pathnameList[i]);
		status = zgetRecordBasics(ifltab1, recordBasics);
		//printf("[%d] \"%s\" %d\n", i, catStruct->pathnameList[i], recordBasics->recordType);
		

		if (recordBasics->recordType == 420)// grid
		{
			zStructSpatialGrid* grid =  zstructSpatialGridNew(catStruct->pathnameList[i]);
			zspatialGridRetrieve(ifltab1, grid, 1);
			if(i%100 == 0)
			   printf(".");
		}
		zstructFree(recordBasics);
		
	}
	double elapsed = (getCurrentTimeMillis() - start_time) / 1000.0;

	printf("\nSeconds elapsed: %f", elapsed);

	zstructFree(catStruct);
	zclose(ifltab1);
	return status;
}

int ImportProfile(const char* csvFilename, const char* dssFilename, const char* path, const char* date, 
	const char* time, const char* units, const char* datatype) {

	int rval = 0;
	zStructTimeSeries* tss = zstructTsNewTimes(path,date,time,"","");
	
	read_profile_from_csv(tss, csvFilename);

	tss->unitsProfileValues = strdup(units);
	tss->allocated[zSTRUCT_TS_profileUnitsValues] = 1;

	tss->unitsProfileDepths = strdup(""); // units for columns
	tss->allocated[zSTRUCT_TS_profileUnitsDepths] = 1;

	tss->type = strdup(datatype);
	tss->allocated[zSTRUCT_TS_type] = 1;

	long long ifltab[250];
	int status = hec_dss_zopen(ifltab, dssFilename);
	if (status != 0)
	{
		rval = -1;
	}
	int storageFlag = 0;  // Always replace data.

	if( status ==0 )
	   ztsStore(ifltab, tss, storageFlag);

	//timeSeriesRecordSizes.dataType == DATA_TYPE_RTS_PROFILE;
	zstructFree(tss);
	zclose(ifltab);
	return 0;
	
}