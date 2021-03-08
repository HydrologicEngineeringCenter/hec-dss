#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "TestDssC.h"

int zaliasList(long long *ifltab, const char* pathname, char** pathnameList, int *pathnameListLength);

	// Tests aliases

int testAlias(long long *ifltab)
{
	zStructTimeSeries *tss1, *tss2, *tss3;
	float fvalues[200];
	char cpath1[100];
	char cpath2[100];
	char *cpath2a;
	char cpath3[100];
	char cpath4[100];
	char cpath5[100];
	char cpath6[100];
	int i;
	int status;
	int zero = 0;
	int count;
	int len;
	char messageString[100];

	char* pathnameList;
	int pathnameListLength;



	if (zgetVersion(ifltab) != 7) return 0;

	for (i=0; i<200; i++) {
		fvalues[i] = (float)i;
	}
	
	stringCopy(cpath1, sizeof(cpath1), "/Alias test/Location/Flow/01Jul2010/1Hour/Primary 1/", _TRUNCATE);
	stringCopy(cpath2, sizeof(cpath2), "/Alias test/l/Flow/01Jul2010/1Hour/Alias 2/", _TRUNCATE);
	stringCopy(cpath3, sizeof(cpath3), "/Alias test/long Location/Flow/01Jul2010/1Hour/Alias #3/", _TRUNCATE);
	stringCopy(cpath4, sizeof(cpath4), "/Alias test/Location, Location, Location/Flow/01Jul2010/1Hour/Alias #4/", _TRUNCATE);
	stringCopy(cpath5, sizeof(cpath5), "/Alias test/nice/Flow/01Jul2010/1Hour/Alias #4/", _TRUNCATE);

	//  Write a record
	tss1 = zstructTsNewRegFloats(cpath1, fvalues, 200, "04July2010", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 1, store status ")) return status; 

	//  Be sure it's there and valid
	tss2 = zstructTsNew(cpath1); 
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 2, retrieve status ")) return status; 

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testAlias, Location 3");
	if (status) return status;

	
	//   Be sure it is a correct pathname (with date proper)
	status = zcheck(ifltab, cpath1);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 3, zcheck status ")) return status; 

	//  Add an alias to that path
	status = zaliasAdd(ifltab, cpath1, cpath2);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 4, zaliasAdd status ")) return status; 

	status = zcheck(ifltab, cpath2);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 5, zcheck status ")) return status; 

	//  Now read in the alias; we should actually be reading the primary
	tss3 = zstructTsNew(cpath2);
	status = ztsRetrieve(ifltab, tss3, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 6, retrieve status ")) return status; 

	//  We have to change the path for the compare to not fail
	cpath2a = tss3->pathname;
	tss3->pathname = tss2->pathname;
	status = zcompareDataSets(ifltab, tss2, tss3, 1, 1, tss2->pathname, "Fail in testAlias, Location 7");
	if (status) return status;
	tss3->pathname = cpath2a;


	/**/
	//   A second alias
	status = zaliasAdd(ifltab, cpath1, cpath3);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 10, zaliasAdd status ")) return status; 
	//  Verify
	status = zcheck(ifltab, cpath3);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 11, zcheck status ")) return status; 

	//   Now an alias point to an alias!	
	status = zaliasAdd(ifltab, cpath3, cpath4);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 12, zaliasAdd status ")) return status; 
	//  Verify
	status = zcheck(ifltab, cpath4);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 13, zcheck status ")) return status; 

	//   A fourth alias
	status = zaliasAdd(ifltab, cpath1, cpath5);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 14, zaliasAdd status ")) return status; 
	//  Verify
	status = zcheck(ifltab, cpath5);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 15, zcheck status ")) return status; 

	//  Now, get a list of the pathnames and make sure we have them all
	status = zaliasList(ifltab, cpath4, &pathnameList, &pathnameListLength);
	if (zcompareInts(ifltab, status, 5, 1, "Fail in testAlias Loc 20, number from pathlist is not 5 ")) return -1; 

	zmessage2(ifltab, "Primary pathname: ", pathnameList);
	if (zcompareStrings(ifltab, cpath1, pathnameList, (int)strlen(cpath1), 1, 1,  "testAlias, primary path does not match, loc 21")) return -1; 
	count = strlen(pathnameList) + 1;
	i=0;
	while (count <pathnameListLength) {
		i++;
		zmessage2(ifltab, "Alias pathname: ", &pathnameList[count]);
		//  Aliases are saved in reverse order (you get the last one first)
		if (i ==4) {
			if (zcompareStrings(ifltab, cpath2, &pathnameList[count], (int)strlen(cpath2), 1, 1,  "testAlias, alias 1 path does not match, loc 22")) return -1; 
		}
		if (i ==3) {
			if (zcompareStrings(ifltab, cpath3, &pathnameList[count], (int)strlen(cpath3), 1, 1,  "testAlias, alias 2 path does not match, loc 23")) return -1; 
		}
		if (i ==2) {
			if (zcompareStrings(ifltab, cpath4, &pathnameList[count], (int)strlen(cpath4), 1, 1,  "testAlias, alias 3 path does not match, loc 24")) return -1; 
		}
		if (i ==1) {
			if (zcompareStrings(ifltab, cpath5, &pathnameList[count], (int)strlen(cpath5), 1, 1,  "testAlias, alias 4 path does not match, loc 25")) return -1; 
		}
		count += strlen(&pathnameList[count]) + 1;
	}

	free(pathnameList);


	zstructFree(tss1);
	zstructFree(tss2);
	zstructFree(tss3);

	//  Now test getting the primary from one of the aliases
	status = zaliasGetPrimary(ifltab, cpath3, cpath6, sizeof(cpath6));
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 30, zaliasGetPrimary status ")) return status; 
	//  Verify
	status = zcheck(ifltab, cpath6);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 31, zcheck status ")) return status; 

	//  Delete that alias (one that is in the middle and breaks the chain
	status = zaliasRemove(ifltab, cpath3);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 32, zaliasRemove status ")) return status; 
	//  Verify that we cannot find alias
	status = zcheck(ifltab, cpath3);
	if (zcompareInts(ifltab, status, STATUS_RECORD_NOT_FOUND, 1, "Fail in testAlias Loc 33")) return -1; 
	//  Check that primary is still there
	status = zcheck(ifltab, cpath6);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 34, zcheck status ")) return status;

	//  Try to delete same alias - should get an error
	zmessage(ifltab, "Force an error...");
	status = zaliasRemove(ifltab, cpath3);
	if (zcompareInts(ifltab, status, STATUS_RECORD_NOT_FOUND, 1, "Fail in testAlias Loc 35")) return -1; 

	//  Get a list and compare
	status = zaliasList(ifltab, cpath4, &pathnameList, &pathnameListLength);
	if (zcompareInts(ifltab, status, 4, 1, "Fail in testAlias Loc 40, number from pathlist is not 4 ")) return -1; 

	zmessage2(ifltab, "Primary pathname: ", pathnameList);
	if (zcompareStrings(ifltab, cpath1, pathnameList, (int)strlen(cpath1), 1, 1,  "testAlias, primary path does not match, loc 41")) return -1; 
	count = strlen(pathnameList) + 1;
	i=0;
	while (count <pathnameListLength) {
		i++;
		zmessage2(ifltab, "Alias pathname: ", &pathnameList[count]);
		//  Aliases are saved in reverse order (you get the last one first)
		if (i ==3) {
			if (zcompareStrings(ifltab, cpath2, &pathnameList[count], (int)strlen(cpath2), 1, 1,  "testAlias, alias 1 path does not match, loc 42")) return -1; 
		}		
		if (i ==2) {
			if (zcompareStrings(ifltab, cpath4, &pathnameList[count], (int)strlen(cpath4), 1, 1,  "testAlias, alias 3 path does not match, loc 44")) return -1; 
		}
		if (i ==1) {
			if (zcompareStrings(ifltab, cpath5, &pathnameList[count], (int)strlen(cpath5), 1, 1,  "testAlias, alias 4 path does not match, loc 45")) return -1; 
		}
		count += strlen(&pathnameList[count]) + 1;
	}

	free(pathnameList);

	zset("MLVL", "", 4);

	//  Now remove all aliases
	status = zaliasRemoveAll(ifltab, cpath4);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 50, zcheck status ")) return status; 
	//  Verify that we cannot find alias
	status = zcheck(ifltab, cpath4);
	if (zcompareInts(ifltab, status, STATUS_RECORD_NOT_FOUND, 1, "Fail in testAlias Loc 51")) return -1; 
	//  Check that primary is still there
	status = zcheck(ifltab, cpath1);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 52, zcheck status ")) return status;

	//  Get a list of aliases; should only have one, the primary
	status = zaliasList(ifltab, cpath1, &pathnameList, &pathnameListLength);
	if (zcompareInts(ifltab, status, 1, 1, "Fail in testAlias Loc 53, number from pathlist is not 1 ")) return -1; 
	free(pathnameList);


	//  Put back a couple of aliases
	status = zaliasAdd(ifltab, cpath1, cpath3);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 60, zaliasAdd status ")) return status; 
	//  Verify
	status = zcheck(ifltab, cpath3);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 61, zcheck status ")) return status; 

	status = zaliasAdd(ifltab, cpath3, cpath4);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 62, zaliasAdd status ")) return status; 
	//  Verify
	status = zcheck(ifltab, cpath4);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 63, zcheck status ")) return status; 

	//  Now "delete" the alias and that should delete both the primary and all aliases (and primary data)
	status = zdelete(ifltab, cpath4);
	if (zcheckStatus(ifltab, status, 1, "Fail in testAlias Loc 64, zdelete status ")) return status; 

	//  Check an alias
	status = zcheck(ifltab, cpath3);
	if (zcompareInts(ifltab, status, STATUS_RECORD_NOT_FOUND, 1, "Fail in testAlias Loc 65")) return -1; 

	//  Check the primary
	status = zcheck(ifltab, cpath1);
	if (zcompareInts(ifltab, status, STATUS_RECORD_NOT_FOUND, 1, "Fail in testAlias Loc 66")) return -1; 

	//  Get the list; should come back null
	status = zaliasList(ifltab, cpath1, &pathnameList, &pathnameListLength);
	if (zcompareInts(ifltab, status, STATUS_RECORD_NOT_FOUND, 1, "Fail in testAlias Loc 67")) return -1;  
	if (pathnameList) {
		zmessage(ifltab, "pathnameList should be zero, but isnt...  Fail in testAlias Loc 68");		
		return -1;
	}
	if (zcompareInts(ifltab, pathnameListLength, 0, 1, "Fail in testAlias Loc 69")) return -1; 


	zmessage(ifltab, "Testing aliases passed successfully");
	return 0; 
}

