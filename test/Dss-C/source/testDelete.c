#include <stdio.h>
#include <string.h>

#include "heclib.h"
#include "hecdssInternal.h"
#include "TestDssC.h"



	//  Test  delete and rename.  Rename is 2nd pass through
	//  

/*
	Testing procedure:
	1.  Write path1 (loc 4)
	2.  Write path2 (loc 11)
	3.  Read path1 (loc 12)
	4.  Read path2 (loc 14)
	5.  Delete path3 - must fail (loc 19)
	6.  Delete path1 (loc 20)
	7.  Read path1 - must fail (loc 21)
	8.  Undelete path1 (loc 22)
	9.  Read path1 (loc 23)
	10. Delete path1 (loc 25)
	11. Write path4 (loc 27)
	12. Read path4 (loc 28)
	13. Undelete path1 - must fail because of path4 overwriting reclaimed space (loc 50)
	14. Delete path2 (loc 52)
	15. Undelete path2 (loc 53)
	
	path1 = "/a/b/c/d/e/1/"
	path2 = "/a/b/c/d/e/2/"
	path3 = "/a/b/c/d/e/3/"
	path4 = "/a/b/c/d/e/4/"
	

*/

int testDelete(const char *dssFilename7, const char *dssFilename6)
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
	long long ifltab[500];
	zStructTransfer *ztransfer1;
	zStructTransfer *ztransfer2;
	zStructTransfer *ztransfer3;
	zStructTransfer *ztransfer4;
	zStructTransfer *ztransfer5;
	zStructTransfer *ztransfer6;

	int values[1000];
	int header[1000];

	for (i=0; i<1000; i++) {
		values[i] = i + 10;
		header[i] = -values[i];
	}

	stringCopy(path1, sizeof(path1), "/a/b/c/d/e/1/", sizeof(path1));
	stringCopy(path2, sizeof(path2), "/a/b/c/d/e/2/", sizeof(path2));
	stringCopy(path3, sizeof(path3), "/a/b/c/d/e/3/", sizeof(path3));
	stringCopy(path4, sizeof(path4), "/a/b/c/d/e/4/", sizeof(path4));
	stringCopy(path5, sizeof(path5), "/a/b/c/d/e/5/", sizeof(path5));
	stringCopy(path6, sizeof(path6), "/this is a longer pathname/b/c/d/e/6/", sizeof(path6));

	//  First pass is for zdelete, second for zrename
	for (iloop=0; iloop<2; iloop++) {

		if (iloop == 0) {
			stringCopy(loopName, sizeof(loopName), "test delete", sizeof(loopName));			
		}
		else {
			stringCopy(loopName, sizeof(loopName), "test rename", sizeof(loopName));
		}
		printf("Running %s\n", loopName);
		//  First loop is for DSS-7, second is for DSS-6

		for (i=0; i<2; i++) {



			//  Delete then open the dss file
			if (i==0) {
				printf("Testing with DSS-7\n");
				remove(dssFilename7);
				status = zopen7(ifltab, dssFilename7);
				sprintf(message, "Fail in %s, zopen Loc 1", loopName);
				if (zcheckStatus(ifltab, status, 1, message)) return status;
			}
			else {			
				printf("Testing with DSS-6\n");
				remove(dssFilename6);
				status = zopen6(ifltab, dssFilename6);
				sprintf(message, "Fail in %s, zopen Loc 2", loopName);
				if (zcheckStatus(ifltab, status, 1, message)) return status;
			}


			//  Write some data to the database
		
			ztransfer1 = zstructTransferNew(path1, 0);
			if (!ztransfer1) {
				printf("Failed to create zStructTransfer in %s, Loc 3!\n", loopName);
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
			ztransfer3->internalHeader = values;
			ztransfer3->internalHeaderNumber = 1000;
			ztransfer3->values1 = header;
			ztransfer3->values1Number = 1000;
			status =  zwrite(ifltab, ztransfer3);
			sprintf(message, "Fail in %s, zwrite Loc 11", loopName);
			if (zcheckStatus(ifltab, status, 1, message)) return status;

			//  Now check that we have the right data
			ztransfer2 = zstructTransferNew(path1, 1);
			status =  zread(ifltab, ztransfer2);
			sprintf(message, "Fail in %s, zread, Loc 12", loopName);
			if (zcheckStatus(ifltab, status, 1, message)) return status;
			status = zcompareDataSets(ifltab, ztransfer1, ztransfer2,  1, 0, path1, "Fail in testDelete, Location 13");
			if (status) return status;
			zstructFree(ztransfer2);

			ztransfer4 = zstructTransferNew(path2, 1);
			status =  zread(ifltab, ztransfer4);
			sprintf(message, "Fail in %s, zread, Loc 14", loopName);
			if (zcheckStatus(ifltab, status, 1, message)) return status;
			sprintf(message, "Fail in %s, Loc 15", loopName);
			status = zcompareDataSets(ifltab, ztransfer3, ztransfer4,  1, 0, path2, message);
			if (status) return status;

			//zset("MLVL", "", 20);
			///  Check Delete
			///  First, try deleting a non-existant record
		
			if (iloop == 0) {
				status = zdelete(ifltab, path3);
				if (status != STATUS_RECORD_NOT_FOUND) {
					printf("Error %s testing non-existant path;  Loc 19!\n", loopName);
					printf("Pathname: %s\n", path3);
					return -1;
				}
			}
			else {
				status = zrename(ifltab, path3, path5);
				if (status != STATUS_RECORD_NOT_FOUND) {
					printf("Error %s testing non-existant path;  Loc 19a!\n", loopName);
					printf("Pathname: %s\n", path3);
					return -1;
				}
			}

			//  Now delete an existing one
			if (iloop == 0) {
				status = zdelete(ifltab, path1);
				sprintf(message, "Fail in %s, Loc 20", loopName);
				if (zcheckStatus(ifltab, status, 1, message)) return status;
			}
			else {				
				status = zrename(ifltab, path1, path5);
				sprintf(message, "Fail in %s, Loc 20a", loopName);
				if (zcheckStatus(ifltab, status, 1, message)) return status;
			}

			//  Make sure it is not there
			ztransfer2 = zstructTransferNew(path1, 1);
			status =  zread(ifltab, ztransfer2);
			if (status == 0) {
				printf ("Read deleted record,  %s fail at Loc 21", loopName);
				return -1;
			}
			zstructFree(ztransfer2);

			//  undelete it. 
			if (iloop == 0) {
				status = zundelete(ifltab, path1);
				sprintf(message, "Fail in %s, Loc 22", loopName);
				if (zcheckStatus(ifltab, status, 1, message)) return status;
			}
			else {
				status = zrename(ifltab, path5, path1);
				sprintf(message, "Fail in %s, Loc 22a", loopName);
				if (zcheckStatus(ifltab, status, 1, message)) return status;
			}

			//  Now check that we have the right data
			ztransfer2 = zstructTransferNew(path1, 1);
			status =  zread(ifltab, ztransfer2);
			sprintf(message, "Fail in %s, Loc 23", loopName);
			if (zcheckStatus(ifltab, status, 1, message)) return status;
			sprintf(message, "Fail in %s, Loc 24", loopName);
			status = zcompareDataSets(ifltab, ztransfer1, ztransfer2,  1, 0, ztransfer1->pathname, message);
			if (status) return status;

			//  Delete again
			if (iloop == 0) {
				status = zdelete(ifltab, path1);
				sprintf(message, "Fail in %s, Loc 25", loopName);
				if (zcheckStatus(ifltab, status, 1, message)) return status;
			}
			else {
				status = zrename(ifltab, path1, path6);
				sprintf(message, "Fail in %s, Loc 25a", loopName);
				if (zcheckStatus(ifltab, status, 1, message)) return status;
			}

			//  Write a different record that is the same size.  This should overwrite the record 
			//  and make the previous one not able to be undeleted
		
			ztransfer5 = zstructTransferNew(path4, 0);
			if (!ztransfer5) {
				printf("Failed to create zStructTransfer in %s, Loc 26!\n", loopName);
				printf("Pathname: %s\n", path4);
				return -1;
			}
			//  Swap values and header for fun
			ztransfer5->internalHeader = values;
			ztransfer5->internalHeaderNumber = 1000;
			ztransfer5->values1 = header;
			ztransfer5->values1Number = 1000;
			status =  zwrite(ifltab, ztransfer5);
			if (zcheckStatus(ifltab, status, 1, "Fail in testDelete, zwrite, Loc 27")) return status;

			//  Now check that we have the right data
			ztransfer6 = zstructTransferNew(path4, 1);
			status =  zread(ifltab, ztransfer6);
			sprintf(message, "Fail in %s, zread, Loc 28", loopName);
			if (zcheckStatus(ifltab, status, 1, message)) return status;
			sprintf(message, "Fail in %s, zread, Loc 29", loopName);
			status = zcompareDataSets(ifltab, ztransfer5, ztransfer6,  1, 0, ztransfer5->pathname, message);
			if (status) return status;
			zstructFree(ztransfer2);


			//  undelete the earlier record.  Shouldn't work for version 7, but should for 6
			if (iloop == 0) {
				status = zundelete(ifltab, path1);
			}
			else {
				status = zrename(ifltab, path1, path4);
			}
		
			
			if ((i == 0) || (iloop == 1)) {
				//errorMessage = zstatus(&errorCode, &severity);			
				//printf("Error message = %s\n", errorMessage);
				if (zgetVersion(ifltab) == 7) {
					if (zinquire(ifltab, "recl") == RECLAIM_NONE) {
						status = -1;
					}
				}
				if (status == STATUS_OKAY) {
					printf("Error %s, should have an error; Loc 50!\n", loopName);
					printf("Pathname: %s\n", path1);
					return -1;
				}
			}
			else {
				sprintf(message, "Fail in %s, vers 6, Loc 51", loopName);
				if (zcheckStatus(ifltab, status, 1, message)) return status;
			}



			//  Now for the second path, delete it and then undelete.  
			//  Should work for both 6 and 7 now.
			if (iloop == 0) {
				status = zdelete(ifltab, path2);
				sprintf(message, "Fail in %s, Loc 52", loopName);
				if (zcheckStatus(ifltab, status, 1, message)) return status;
				status = zundelete(ifltab, path2);
				sprintf(message, "Fail in %s, Loc 53", loopName);
				if (zcheckStatus(ifltab, status, 1, message)) return status;
			}

			//  Check undeleteAll

			zstructFree(ztransfer1);		
			zstructFree(ztransfer3);
			zstructFree(ztransfer4);
			zstructFree(ztransfer5);
			zstructFree(ztransfer6);

			status = zclose(ifltab);
			sprintf(message, "Fail in %s, zclose Loc 55", loopName);
			if (zcheckStatus(ifltab, status, 1, message)) return status;

		}

		if (iloop == 0) {
			printf("Completed test delete successfully!\n");
		}
		else {
			printf("Completed test rename successfully!\n");
		}
	}

	

	return 0;
}
