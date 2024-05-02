#include <stdio.h>
#include <string.h>

#include "heclib.h"
#include "hecdss7.h"
#include "hecdssInternal.h"
#include "TestDssC.h"


//  Tests catalog functions for both DSS-7 and DSS-6
//  Requires the existance of sample7.dss and sample6.dss

//  Tests the following functions:
//		int zcatalog(long long *ifltab, zStructCatalog *catStruct, int boolSorted, int boolCondensed)
//		int zcatalogSearch(long long *ifltab, char *pathWithWild, zStructCatalog *catStruct, int boolSorted, int boolCondensed)
//		int zcatalogFile(long long *ifltab, const char *dssFilename, int boolSorted, int boolCondensed)

int testCatalog()
{
	int status;
	int len;

	long long ifltab[600] = {0};

	zStructCatalog *catStruct;
	zStructCatalog *catStruct2;
	zStructTransfer* ztransfer;
	char dssFilename[_MAX_PATH] = {0};
	char filename[_MAX_PATH ] = {0};
	char catFilename[_MAX_PATH] = {0};
	char pathWithWild[100] = {0};
	char pathname[200] = {0};
	int filePos;
	int nPathname;
	int count;
	int i;
	int permission;
	int boolSame;
	int buffer[1];
	long long bufferControl[4] ={0};
	long long lastWriteTime;


/*
	//  Use c:/temp/sample7.dss, the standard sample.dss file from DSS-Vue 2.0, converted to 7.
	stringCopy(dssFilename, sizeof(dssFilename), "c:/temp/coll.dss", _TRUNCATE);

	//  Make sure it exists and is version 7
	if (zgetFileVersion(dssFilename) != 7) {
		status = zcompareInts(ifltab, zgetFileVersion(dssFilename), 7, 1,  "testCatalog: c:/temp/sample7.dss does not exist or is not version 7 ");
		if (status != STATUS_OKAY) return status;
	}
	
	//  Open the file
	status = hec_dss_zopen(ifltab, dssFilename);
	if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 2, zopen status ")) return status; 

	//  Get a complete catalog (all pathnames in the DSS file)
	catStruct = zstructCatalogNew();
	status = zcollectionCat((long long*)ifltab, "/Without Project Conditions-TimeWindow/FIA-ALT_Grids/Event-Area/Output/Total Acreage Flooded/C:000000|Without Pr:TimeWindow:FIA-ALT_Grids/", catStruct);
	if (status < 0) {
		if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 3, zcatalog status ")) return status; 
	}
	zstructFree(catStruct);
	
	catStruct = zstructCatalogNew();
	catStruct->boolIncludeDates = 1;
	status = zcatalog(ifltab, (const char *)0, catStruct, 1);	
	if (status < 0) {
		if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 4, zcatalog status ")) return status; 
	}
	zstructFree(catStruct);
	*/

	////////////////////////////////////////////////

	stringCopy(dssFilename, sizeof(dssFilename), "sample7.dss", _TRUNCATE);
	

	//  Make sure it exists and is version 7
	zfileName(filename, sizeof(filename), dssFilename, &permission);
	if (permission != 0) {
		printf("Squeeze for file %s does not have appropriate permssion: %d\n", dssFilename, permission);
		return -1;
	}
	
	if (zgetFileVersion(dssFilename) != 7) {
		status = zcompareInts(ifltab, zgetFileVersion(dssFilename), 7, 1,  "testCatalog: sample7.dss does not exist or is not version 7 ");
		if (status != STATUS_OKAY) return status;
	}

	//  Make sure the last write times are correct for these tests
	printf("squeezing file to be sure last write time are correct for catalog test ->%s<-\n", filename);
	//zset("mlvl", "", 15);
	status = zsqueeze(filename);
	if (status != STATUS_OKAY) {
		printf("test Catalog failed in squeeze, status = %d\n", status);
		return status;
	}
	
	//  Open the file
	status = hec_dss_zopen(ifltab, dssFilename);
	if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 2, zopen status ")) return status; 

	//  Get a complete catalog (all pathnames in the DSS file)
	catStruct = zstructCatalogNew();
	status = zcatalog(ifltab, (const char *)0, catStruct, 0);
	if (status < 0) {
		if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 3, zcatalog status ")) return status; 
	}
	printf("\nfound %d records in catalog ", catStruct->numberPathnames);

	zstructFree(catStruct);
	
	catStruct = zstructCatalogNew();
	catStruct->boolIncludeDates = 1;
	printf("\nreading catlog with dates catStruct->boolIncludeDates = 1");
	status = zcatalog(ifltab, (const char *)0, catStruct, 1);	
	if (status < 0) {
		if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 4, zcatalog status ")) return status; 
	}
	printf("\nfound %d records in catalog ", catStruct->numberPathnames);
	zstructFree(catStruct);

	//  Now test for wild characters
	stringCopy(pathWithWild, sizeof(pathWithWild), "//SACRAMENTO/*/*/*/OBS/", _TRUNCATE);
	catStruct = zstructCatalogNew();
	catStruct->boolIncludeDates = 1;
	printf("\nreading catalog with filter '//SACRAMENTO/*/*/*/OBS/' ");
	status = zcatalog(ifltab, pathWithWild, catStruct, 1);
	if (status < 0) {
		zstructFree(catStruct);
		if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 6, zcatalog status ")) return status; 
	}
	printf("\nfound %d records in catalog ", catStruct->numberPathnames);
	zstructFree(catStruct);

	//  Now, a catalog file
	stringCopy(catFilename, sizeof(catFilename), dssFilename, _TRUNCATE);
	len = (int)strlen(catFilename);
	catFilename[len-1] = 'c';
	remove(catFilename);
	printf("\nwriting catalog to %s ", catFilename);
	status = zcatalogFile(ifltab, catFilename, 1, (const char *)0);
	if (status < 0) {
		if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 7, zcatalog status ")) return status; 
	}
	
	
	//  Version 6 code on vers 7 file
	printf("\nreading catalog with legacy(v6) filter  'B=SACRAMENTO, F=OBS' ");
	stringCopy(pathWithWild, sizeof(pathWithWild), "B=SACRAMENTO, F=OBS", _TRUNCATE);
	printf("\n%s", pathWithWild);
	filePos = 0;
	count = 0;
	while (filePos >= 0) {
		zplist_ (ifltab, pathWithWild, &filePos, pathname,
               &nPathname, &status, strlen(pathWithWild), sizeof(pathname));
		if (status != STATUS_OKAY) {
			if (filePos < 0) break;
			if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 8, zcatalog status ")) return status; 
		}
		//printf(" %d,  -->%s<--\n", count, pathname);
		count++;
	}

	//  
	//  Test "What has changed?"
	if (zgetVersion(ifltab) == 7) {

		
		//  Test the data CRC function; assume okay if values > 0
		catStruct = zstructCatalogNew();
		printf("\nTesting with catStruct->boolGetCRCvalues = 1");
		catStruct->boolGetCRCvalues = 1;
		status = zcatalog(ifltab, (const char*)0, catStruct, 0);
		
		if (status < 0) {
			zstructFree(catStruct);
			if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 40, zcatalog CRC ")) return status; 
		}
		printf("\nfound %d records ", catStruct->numberPathnames);
		for (i=0; i<catStruct->numberPathnames; i++) {
			if (catStruct->crcValues[i] < 1) {
				zstructFree(catStruct);
				if (zcheckStatus(ifltab, -1, 1, "Fail in testCatalog Loc 41, zcatalog CRC values ")) return status; 
			}
		}
		zstructFree(catStruct);

		catStruct = zstructCatalogNew();
		catStruct->boolGetCRCvalues = 1;
		status = zcatalog(ifltab, (const char*)0, catStruct, 0);
		if (status < 0) {
			zstructFree(catStruct);
			if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 10, zcatalog status ")) return status; 
		}
		lastWriteTime = catStruct->lastWriteTimeFile;
		//  Do some stuff

		if (catStruct->numberPathnames > 100) {
			//  Choose pathnames 70-79 to "update"
			for (i=70; i<80; i++) {
				ztransfer = zstructTransferNew(catStruct->pathnameList[i], 1);
				status = zreadInternal(ifltab, ztransfer, bufferControl, buffer, 0); 
				if (status < 0) {
					zstructFree(catStruct);
					if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 11, zcatalog status ")) return status; 
				}
				status = zwriteInternal(ifltab, ztransfer, 0, bufferControl, buffer, 0); 
				if (status < 0) {
					zstructFree(catStruct);
					if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 12, zcatalog status ")) return status; 
				}				
				zstructFree(ztransfer);
			}

			catStruct2 = zstructCatalogNew();
			//  Set last write time search and flag
			catStruct2->lastWriteTimeSearch = lastWriteTime;
			catStruct2->lastWriteTimeSearchFlag = 2;
			status = zcatalog(ifltab, (const char*)0, catStruct2, 0);
			if (status < 0) {
				zstructFree(catStruct);
				zstructFree(catStruct2);
				if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 13, zcatalog status ")) return status; 
			}

			//  This new struct should have 10 pathnames, the ones from just above.
			if (catStruct2->numberPathnames != 10) {
				printf("Error - catalog length should be 10, but it is %d\n", catStruct2->numberPathnames);
				if (zcheckStatus(ifltab, -1, 1, "Fail in testCatalog Loc 14, zcatalog not 10 records long")) return -1; 
			}

			//  Verify that they are the same records
			count = 0;
			for (i=70; i<80; i++) {
				if (catStruct->pathnameHash[i] != catStruct2->pathnameHash[count]) {
					printf("Error - catalog hashes should be the same, but they are not.\n");
					printf("Pathname 1: -->%s<--\n", catStruct->pathnameList[i]);
					printf("Pathname 2: -->%s<--\n", catStruct2->pathnameList[count]);
					if (zcheckStatus(ifltab, -1, 1, "Fail in testCatalog Loc 15, different pathnames")) return -1; 
				}
				boolSame = zstringCompare(catStruct->pathnameList[i], catStruct2->pathnameList[count], strlen(catStruct->pathnameList[i]));
				if (!boolSame) {
					printf("Error - catalog pathnames should be the same, but they are not.\n");
					printf("Pathname 1: -->%s<--\n", catStruct->pathnameList[i]);
					printf("Pathname 2: -->%s<--\n", catStruct2->pathnameList[count]);
					if (zcheckStatus(ifltab, -1, 1, "Fail in testCatalog Loc 16, different pathnames")) return -1; 
				}
				count++;
			}
			zstructFree(catStruct2);

			//  Now, essentially the same code in the zwhatsChange method
			//zsetMessageLevel(MESS_METHOD_CATALOG_ID, MESS_LEVEL_INTERNAL_DIAG_2);
			catStruct2 = zstructCatalogNew();
			count = zwhatChangedCompare(ifltab, catStruct, catStruct2, (const char*)0, 1);
			if (count != 0) {
				printf("Error - catalog count should be 0, but it is %d\n", count);
				if (catStruct2->numberPathnames > 0) {
					count = catStruct2->numberPathnames;
					if (count > 10) count = 10;
					for (i = 0; i < count; i++) {
						printf(" %d, path = %s\n", i, catStruct2->pathnameList[i]);
					}
				}
				if (zcheckStatus(ifltab, -1, 1, "Fail in testCatalog Loc 21, zcatalog not 0 records long")) return -1; 
			}
			for (i=70; i<80; i++) {
				ztransfer = zstructTransferNew(catStruct->pathnameList[i], 1);
				status = zreadInternal(ifltab, ztransfer, bufferControl, buffer, 0); 
				if (status < 0) {
					zstructFree(catStruct);
					if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 11, zcatalog status ")) return status; 
				}
				//  Change the data
				ztransfer->values1[2] = 2;
				status = zwriteInternal(ifltab, ztransfer, 0, bufferControl, buffer, 0); 
				if (status < 0) {
					zstructFree(catStruct);
					if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 12, zcatalog status ")) return status; 
				}				
				zstructFree(ztransfer);
			}
			zstructFree(catStruct2);
			catStruct2 = zstructCatalogNew();
			count = zwhatChangedCompare(ifltab, catStruct, catStruct2, (const char*)0, 1);
			//  This new struct should have 10 pathnames, the ones from just above.
			if (catStruct2->numberPathnames != 10) {
				printf("Error - catalog length should be 10, but it is %d\n", catStruct2->numberPathnames);
				if (zcheckStatus(ifltab, -1, 1, "Fail in testCatalog Loc 20, zcatalog not 10 records long")) return -1; 
			}
			if (count != 10) {
				printf("Error - catalog count should be 10, but it is %d\n", count);
				if (zcheckStatus(ifltab, -1, 1, "Fail in testCatalog Loc 21, zcatalog not 10 records long")) return -1; 
			}

			//  Verify that they are the same records
			count = 0;
			for (i=70; i<80; i++) {
				if (catStruct->pathnameHash[i] != catStruct2->pathnameHash[count]) {
					printf("Error - catalog hashes should be the same, but they are not.\n");
					printf("Pathname 1: -->%s<--\n", catStruct->pathnameList[i]);
					printf("Pathname 2: -->%s<--\n", catStruct2->pathnameList[count]);
					if (zcheckStatus(ifltab, -1, 1, "Fail in testCatalog Loc 22, different pathnames")) return -1; 
				}
				boolSame = zstringCompare(catStruct->pathnameList[i], catStruct2->pathnameList[count], strlen(catStruct->pathnameList[i]));
				if (!boolSame) {
					printf("Error - catalog pathnames should be the same, but they are not.\n");
					printf("Pathname 1: -->%s<--\n", catStruct->pathnameList[i]);
					printf("Pathname 2: -->%s<--\n", catStruct2->pathnameList[count]);
					if (zcheckStatus(ifltab, -1, 1, "Fail in testCatalog Loc 23, different pathnames")) return -1; 
				}
				count++;
			}
			zstructFree(catStruct2);
	
			//  Check boolCRC
			zwhatChangedSetStart(ifltab, (zStructCatalog*)0, (const char*)0, 1);

			for (i=70; i<80; i++) {
				ztransfer = zstructTransferNew(catStruct->pathnameList[i], 1);
				status = zreadInternal(ifltab, ztransfer, bufferControl, buffer, 0); 
				if (status < 0) {
					zstructFree(catStruct);
					if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 24, zcatalog status ")) return status; 
				}
				status = zwriteInternal(ifltab, ztransfer, 0, bufferControl, buffer, 0); 
				if (status < 0) {
					zstructFree(catStruct);
					if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 25, zcatalog status ")) return status; 
				}				
				zstructFree(ztransfer);
			}
			catStruct2 = zstructCatalogNew();
			count = zwhatChanged(ifltab, catStruct2);
			if (count != 0) {
				printf("Error - catalog count should be 0, but it is %d\n", count);
				if (zcheckStatus(ifltab, -1, 1, "Fail in testCatalog Loc 26, zcatalog not 0 records long")) return -1; 
			}
			//  Now re-run, changing the data
			for (i=70; i<80; i++) {
				ztransfer = zstructTransferNew(catStruct->pathnameList[i], 1);
				status = zreadInternal(ifltab, ztransfer, bufferControl, buffer, 0); 
				if (status < 0) {
					zstructFree(catStruct);
					if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 27, zcatalog status ")) return status; 
				}
				//  Change the data
				ztransfer->values1[2] = 7;
				status = zwriteInternal(ifltab, ztransfer, 0, bufferControl, buffer, 0); 
				if (status < 0) {
					zstructFree(catStruct);
					if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 28, zcatalog status ")) return status; 
				}				
				zstructFree(ztransfer);
			}
			zstructFree(catStruct2);
			catStruct2 = zstructCatalogNew();
			count = zwhatChanged(ifltab, catStruct2);
			//  This new struct should have 10 pathnames, the ones from just above.
			if (catStruct2->numberPathnames != 10) {
				printf("Error - catalog length should be 10, but it is %d\n", catStruct2->numberPathnames);
				if (zcheckStatus(ifltab, -1, 1, "Fail in testCatalog Loc 29, zcatalog not 10 records long")) return -1; 
			}
			if (count != 10) {
				printf("Error - catalog count should be 10, but it is %d\n", count);
				if (zcheckStatus(ifltab, -1, 1, "Fail in testCatalog Loc 30, zcatalog not 10 records long")) return -1; 
			}

			//  Verify that they are the same records
			count = 0;
			for (i=70; i<80; i++) {
				if (catStruct->pathnameHash[i] != catStruct2->pathnameHash[count]) {
					printf("Error - catalog hashes should be the same, but they are not.\n");
					printf("Pathname 1: -->%s<--\n", catStruct->pathnameList[i]);
					printf("Pathname 2: -->%s<--\n", catStruct2->pathnameList[count]);
					if (zcheckStatus(ifltab, -1, 1, "Fail in testCatalog Loc 32, different pathnames")) return -1; 
				}
				boolSame = zstringCompare(catStruct->pathnameList[i], catStruct2->pathnameList[count], strlen(catStruct->pathnameList[i]));
				if (!boolSame) {
					printf("Error - catalog pathnames should be the same, but they are not.\n");
					printf("Pathname 1: -->%s<--\n", catStruct->pathnameList[i]);
					printf("Pathname 2: -->%s<--\n", catStruct2->pathnameList[count]);
					if (zcheckStatus(ifltab, -1, 1, "Fail in testCatalog Loc 33, different pathnames")) return -1; 
				}
				count++;
			}
			zstructFree(catStruct2);

		}

		zstructFree(catStruct);


	}

	zclose(ifltab);
	//printf("***********  CAT 6 not tested ****************\n");
	//return 0;
	

	//  Now repeat the tests with DSS-6 (sample6.dss)
	stringCopy(dssFilename, sizeof(dssFilename), "sample6.dss", _TRUNCATE);

	//  Make sure it exists and is version 7
	if (zgetFileVersion(dssFilename) != 6) {
		status = zcompareInts(ifltab, zgetFileVersion(dssFilename), 6, 1,  "testCatalog: c:/temp/sample6.dss does not exist or is not version 6 ");
		if (status != STATUS_OKAY) return status;
	}
	
	//  Open the file
	status = hec_dss_zopen(ifltab, dssFilename);
	if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 42, zopen status ")) return status; 

	//  Get a complete catalog (all pathnames in the DSS file)
	catStruct = zstructCatalogNew();
	status = zcatalog(ifltab, (const char *)0, catStruct, 0);
	if (status < 0) {
		if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 43, zcatalog status ")) return status; 
	}
	zstructFree(catStruct);

	catStruct = zstructCatalogNew();
	catStruct->boolIncludeDates = 1;
	status = zcatalog(ifltab, (const char *)0, catStruct, 1);
	if (status < 0) {
		if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 44, zcatalog status ")) return status; 
	}
	zstructFree(catStruct);


	//  Now test for wild characters
	stringCopy(pathWithWild, sizeof(pathWithWild), "//SACRAMENTO/*/*/*/OBS/", _TRUNCATE);
	catStruct = zstructCatalogNew();
	catStruct->boolIncludeDates = 1;
	status = zcatalog(ifltab, pathWithWild, catStruct, 1);
	if (status < 0) {
		if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 46, zcatalog status ")) return status; 
	}
	zstructFree(catStruct);

	//  Now, a catalog file
	stringCopy(catFilename, sizeof(catFilename), dssFilename, _TRUNCATE);
	len = (int)strlen(catFilename);
	catFilename[len-1] = 'c';
	remove(catFilename);
	status = zcatalogFile(ifltab, dssFilename, 1, (const char *)0);
	if (status < 0) {
		if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 47, zcatalog status ")) return status; 
	}


	//  Version 6 code
	stringCopy(pathWithWild, sizeof(pathWithWild), "B=SACRAMENTO, F=OBS", _TRUNCATE);
	filePos = 0;
	count = 0;
	stringFill(pathname, ' ', sizeof(pathname));
	while (filePos >= 0) {
		zplist_ (ifltab, pathWithWild, &filePos, pathname,
               &nPathname, &status, strlen(pathWithWild), sizeof(pathname));
		if (status == 1) break;
		if (status != STATUS_OKAY) {
			if (zcheckStatus(ifltab, status, 1, "Fail in testCatalog Loc 48, zcatalog status ")) return status; 
		}
		pathname[nPathname] = '\0';
		//printf(" %d,  -->%s<--\n", count, pathname);
		count++;
	}


	zclose(ifltab);

	printf("Catalog testing complete\n");


	return 0;
}