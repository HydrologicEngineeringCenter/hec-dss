#include <stdio.h>
#include <string.h>

#include "heclib.h"
#include "hecdss7.h"
#include "hecdssInternal.h"
#include "TestDssC.h"


//  Tests catalog functions for  DSS-6
//  Requires the existance of sample7.dss and sample6.dss

//  Tests the following functions:
//		int zcatalog(long long *ifltab, zStructCatalog *catStruct, int boolSorted, int boolCondensed)
//		int zcatalogSearch(long long *ifltab, char *pathWithWild, zStructCatalog *catStruct, int boolSorted, int boolCondensed)
//		int zcatalogFile(long long *ifltab, const char *dssFilename, int boolSorted, int boolCondensed)

int testCatalog6()
{
	int DEBUG=0;
	int status;
	int len;

	long long ifltab[600];

	zStructCatalog *catStruct;
	zStructCatalog *catStruct2;
	zStructCatalog *catStruct3;
	zStructTransfer* ztransfer;
	char dssFilename[50];
	char catFilename[50];
	char pathWithWild[100];
	char pathname[200];
	int filePos;
	int nPathname;
	int count;
	int i;
	int boolSame;
	int buffer[1]; long long bufferControl[4] ={0,0,0,0};
	long long lastWriteTime;


	//  Now repeat the tests with DSS-6 (sample6.dss)
	stringCopy(dssFilename, sizeof(dssFilename), "sample6.dss", _TRUNCATE);

	//  Make sure it exists and is version 7
	if (zgetFileVersion(dssFilename) != 6) {
		status = zcompareInts(ifltab, zgetFileVersion(dssFilename), 6, 1,  "testCatalog: sample6.dss does not exist or is not version 6 ");
		if (status != STATUS_OKAY) return status;
	}
	
	//  Open the file
	status = zopen(ifltab, dssFilename);
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
		//printf(" %d,  %s\n", count, pathname);
		count++;
	}


	zclose(ifltab);


	return 0;
}
