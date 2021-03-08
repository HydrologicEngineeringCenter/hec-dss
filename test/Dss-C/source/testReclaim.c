#include <stdio.h>
#include <string.h>

#include "heclib.h"
#include "hecdssInternal.h"
#include "TestDssC.h"



	//  Test  reclaim space
	//  See Fortran test for large file reclaim



int testReclaim(const char *dssFilename)
{
	char path1[MAX_PATHNAME_LENGTH];
	char path2[MAX_PATHNAME_LENGTH];
	char path3[MAX_PATHNAME_LENGTH];
	char path4[MAX_PATHNAME_LENGTH];
	char path5[MAX_PATHNAME_LENGTH];
	char path6[MAX_PATHNAME_LENGTH];

	char loopName[30];
	char message[80];

	int status;
	int i;
	int iloop;
	long long space;
	long long space2;
	long long ifltab[500];
	zStructTransfer *ztransfer1;
	zStructTransfer *ztransfer2;
	zStructTransfer *ztransfer3;
	zStructTransfer *ztransfer4;
	zStructTransfer *ztransfer5;
	zStructTransfer *ztransfer6;

	int values[2000];
	int header[2000];


	for (i=0; i<2000; i++) {
		values[i] = i + 10;
		header[i] = -values[i];
	}

	stringCopy(path1, sizeof(path1), "/a/b/c/d/e/1/", sizeof(path1));
	stringCopy(path2, sizeof(path2), "/a/b/c/d/e/2/", sizeof(path2));
	stringCopy(path3, sizeof(path3), "/a/b/c/d/e/3/", sizeof(path3));
	stringCopy(path4, sizeof(path4), "/a/b/c/d/e/4/", sizeof(path4));
	stringCopy(path5, sizeof(path5), "/a/b/c/d/e/5/", sizeof(path5));
	stringCopy(path6, sizeof(path6), "/this is a longer pathname/b/c/d/e/6/", sizeof(path6));

	stringCopy(loopName, sizeof(loopName), "test reclaim", sizeof(loopName));	


	remove(dssFilename);
	status = zopen7(ifltab, dssFilename);
	sprintf(message, "Fail in %s, zopen Loc 1", loopName);
	if (zcheckStatus(ifltab, status, 1, message)) return status;
	zsetFile(ifltab, "recl", "", RECLAIM_ALL);

	//  Write some data to the database
	ztransfer1 = zstructTransferNew(path1, 0);
	if (!ztransfer1) {
		printf("Failed to create zStructTransfer, Loc 3!\n");
		printf("Pathname: %s\n", path1);
		return -1;
	}

	ztransfer1->internalHeader = header;
	ztransfer1->internalHeaderNumber = 1000;
	ztransfer1->values1 = values;
	ztransfer1->values1Number = 1000;
	status =  zwrite(ifltab, ztransfer1);
	sprintf(message, "Fail in %s, zwrite Loc 4", loopName);
	if (zcheckStatus(ifltab, status, 1, message)) return status;

	//  More data
		
	ztransfer3 = zstructTransferNew(path2, 0);
	if (!ztransfer3) {
		printf("Failed to create zStructTransfer in %s, Loc 10!\n", loopName);
		printf("Pathname: %s\n", path2);
		return -1;
	}
	//  Swap values and header for fun
	//  transfer 3 is twice as big as 1
	ztransfer3->internalHeader = values;
	ztransfer3->internalHeaderNumber = 2000;
	ztransfer3->values1 = header;
	ztransfer3->values1Number = 2000;
	status =  zwrite(ifltab, ztransfer3);
	sprintf(message, "Fail in %s, zwrite Loc 11", loopName);
	if (zcheckStatus(ifltab, status, 1, message)) return status;

	//  Now check that we have the right data
	ztransfer2 = zstructTransferNew(path1, 1);
	status =  zread(ifltab, ztransfer2);
	sprintf(message, "Fail in %s, zread, Loc 12", loopName);
	if (zcheckStatus(ifltab, status, 1, message)) return status;
	status = zcompareDataSets(ifltab, ztransfer1, ztransfer2,  1, 0, path1, "Fail in test reclaim, Location 13");
	if (status) return status;
	zstructFree(ztransfer2);

	ztransfer4 = zstructTransferNew(path2, 1);
	status =  zread(ifltab, ztransfer4);
	sprintf(message, "Fail in %s, zread, Loc 14", loopName);
	if (zcheckStatus(ifltab, status, 1, message)) return status;
	sprintf(message, "Fail in %s, Loc 15", loopName);
	status = zcompareDataSets(ifltab, ztransfer3, ztransfer4,  1, 0, path2, message);
	if (status) return status;
	zstructFree(ztransfer4);

	//  Delete the first data set
	status = zdelete(ifltab, path1);
	sprintf(message, "Fail in %s, Loc 20", loopName);
	if (zcheckStatus(ifltab, status, 1, message)) return status;
	

	//  Make sure it is not there
	ztransfer5 = zstructTransferNew(path1, 1);
	status =  zread(ifltab, ztransfer5);
	if (status == 0) {
		printf ("Read deleted record,  %s fail at Loc 21", loopName);
		return -1;
	}
	zstructFree(ztransfer5);

	//  See how much dead space we have
	space = zinquire(ifltab, "dsiz");
	printf ("Amount of dead space after first delete: %lld\n", space);

	//  Re-write the same record....   should use the same space
	status =  zwrite(ifltab, ztransfer1);
	sprintf(message, "Fail in %s, zwrite Loc 22", loopName);
	if (zcheckStatus(ifltab, status, 1, message)) return status;

	//  Verify that it is there
	ztransfer2 = zstructTransferNew(path1, 1);
	status =  zread(ifltab, ztransfer2);
	sprintf(message, "Fail in %s, zread, Loc 23", loopName);
	if (zcheckStatus(ifltab, status, 1, message)) return status;
	status = zcompareDataSets(ifltab, ztransfer1, ztransfer2,  1, 0, path1, "Fail in test reclaim, Location 24");
	if (status) return status;
	zstructFree(ztransfer2);

	//  Now  see how much dead space we have
	space2 = zinquire(ifltab, "dsiz");
	printf ("Amount of dead space after re-write: %lld\n", space2);

	if (space2 != 0) {
		printf ("Check reclaim, space should be at 0, reported is %lld\n", space2);
		return -1;
	}

	//  Now we'll delete the first dataset again, then write the thrid, which is bigger
	//  and will not reclaim
	//  Delete the first data set
	status = zdelete(ifltab, path1);
	sprintf(message, "Fail in %s, Loc 40", loopName);
	if (zcheckStatus(ifltab, status, 1, message)) return status;
	

	//  Make sure it is not there
	ztransfer5 = zstructTransferNew(path1, 1);
	status =  zread(ifltab, ztransfer5);
	if (status == 0) {
		printf ("Read deleted record,  %s fail at Loc 41", loopName);
		return -1;
	}
	zstructFree(ztransfer5);

	//  See how much dead space we have
	space = zinquire(ifltab, "dsiz");
	printf ("Amount of dead space after first delete: %lld\n", space);

	//  Write the larger record....  
	if (ztransfer3->pathname) free(ztransfer3->pathname);
	ztransfer3->pathname = mallocAndCopy(path6);
	ztransfer3->pathnameLength = 0;
	status =  zwrite(ifltab, ztransfer3);
	sprintf(message, "Fail in %s, zwrite Loc 42", loopName);
	if (zcheckStatus(ifltab, status, 1, message)) return status;
	

	//  Verify that it is there
	ztransfer2 = zstructTransferNew(path6, 1);
	status =  zread(ifltab, ztransfer2);
	sprintf(message, "Fail in %s, zread, Loc 43", loopName);
	if (zcheckStatus(ifltab, status, 1, message)) return status;
	status = zcompareDataSets(ifltab, ztransfer2, ztransfer3,  1, 0, path1, "Fail in test reclaim, Location 44");
	if (status) return status;
	zstructFree(ztransfer2);
	zstructFree(ztransfer3);

	//  Now  see how much dead space we have
	space2 = zinquire(ifltab, "dsiz");
	printf ("Amount of dead space after large write (should be > 0): %lld\n", space2);
	if (space2 == 0) {
		printf ("Check reclaim, space should > 0, reported is %lld\n", space2);
		return -1;
	}
	
	zstructFree(ztransfer1);
	zclose(ifltab);
	
	printf ("Passed simple reclamation tests (Fortan tests are more extensive.)\n");

	return 0;
}
