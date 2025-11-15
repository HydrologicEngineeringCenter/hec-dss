#include "stdio.h"
#include "string.h"
#include <time.h>
#include <stdlib.h>
//#include <windows.h>

#include "heclib.h"
#include "hecdss7.h"
#include "hecdssInternal.h"
#include "zdssKeys.h"
#include "TestDssC.h"



int testMultiUser(char *filename, int version, int numberDatasets, int accessMode /*2=multi, 3=single, 4=exclusive*/)
{
	int status;
	long long ifltab[250];
	int i, j, n;

	clock_t begin, end;
    memset(ifltab,0,sizeof(ifltab));
	zStructCatalog *catStruct;
//	int idigit;
	//char filename[50];
	//char logfile[50];
//	int numberDatasets;
	int sizeOfRecord;
	float *data;
	float *readData;
	int npaths;
	zStructTransfer* ztransfer;
	char pathname[MAX_PATHNAME_LENGTH];

	
	const char *apart = "testmultiuser";

	//////////////////////////////////////////////
	/////////  Set number of records to write here
	//numberDatasets = 100;
	/////////  Set the size of each record here
	sizeOfRecord = 10001;
	//////////////////////////////////////////////

	//  Require the file "sample7.dss"
	//stringCopy(filename, sizeof(filename), "sample7.dss", _TRUNCATE);

	printf("\n\n    ----   DSS Multi User Test   ----\n");


	/*printf("Press 3 for single user access, 2 for multi-user access, 4 for exclusive, or other digit to stop\n\n==>");
	scanf("%d", &idigit);*/
	if (accessMode < 1) {
		printf("\nExiting....\n");
		return -1;
	}
	if (accessMode > 4) {
		printf("\nExiting....\n");
		return -1;
	}

	//printf("Enter the A part\n");
	//scanf("%s", apart);

	/*stringCopy(logfile, sizeof(logfile), "", _TRUNCATE);
	strncat_s(logfile, sizeof(logfile), &apart, _TRUNCATE);
	strncat_s(logfile, sizeof(logfile), ".log", _TRUNCATE);
*/
	zopenLog("testmultiuser.log");

	printf("Log File: testmultiuser.log\n");


	//  Open the file
	status = zopenInternal(ifltab, filename, accessMode, 0, 0, 0, 0);

	if (status != STATUS_OKAY) return status;



	//printf("Enter the number of records to write\n");
	//scanf("%d", &numberDatasets);	


	//  allocate sizeOfRecord floats
	data = (float *)calloc(sizeOfRecord, 4);
	readData = (float *)calloc(sizeOfRecord, 4);
	//  Set the values
	for (i = 0; i<sizeOfRecord; i++) {
		data[i] = (float)i;
	}

	//  We'll use the base struct to write, but mimic array write (which can be displayed in DSSVue)
	ztransfer = zstructTransferNew((const char *)0, 0);

	//  First write to give us a chance to start another process
	sprintf(pathname, "/First write/Size %d/c/d/e/number %d/", sizeOfRecord, 0);
	ztransfer->pathname = pathname;
	ztransfer->pathnameLength = (int)strlen(pathname);
	ztransfer->values1 = (int *)data;
	ztransfer->values1Number = sizeOfRecord;
	ztransfer->numberValues = sizeOfRecord;	//  (total values, in this case same)
	status = zwrite(ifltab, ztransfer);

	//  Turn message level down (IO to console can influence
	//zsetMessageLevel(MESS_METHOD_GENERAL_ID, MESS_LEVEL_TERSE);
	//zsetMessageLevel(MESS_METHOD_GENERAL_ID, MESS_LEVEL_USER_DIAG);


	/*printf("First write complete.\nEnter a 1 to begin time test, 0 to close / open\n");
	scanf("%d", &i);
	if (i == 0) {
		zcheckAccessReset(ifltab, 0, 5000);
		printf("Enter a digit to begin time test\n");
		scanf("%d", &i);
	}
*/

	//  Set start time
	begin = clock();
 

	printf("\nGo!\n");

	//  write and read x records of 10,000 values each
	ztransfer->dataType = DATA_TYPE_FLOAT_ARRAY;
	for (i = 0; i<numberDatasets; i++) {
		sprintf(pathname,"/%s/Size %d/c/d/e/number %d/", apart, sizeOfRecord, i);
		ztransfer->pathname = pathname;
		ztransfer->pathnameLength = (int)strlen(pathname);
		ztransfer->values1 = (int *)data;
		ztransfer->values1Number = sizeOfRecord;
		ztransfer->numberValues = sizeOfRecord;	//  (total values, in this case same)
		status = zwrite(ifltab, ztransfer);
		if (status != STATUS_OKAY) {
			printf("Error in write, status = %d\n", status);
			return status;
		}
		n = i / 200;
		j = n * 200;
		if ((j == i) && (i > 0)) {
			 
			printf("record  %d \n",i );
			//Sleep(100L);
		}
		//printf("Enter a digit to continue\n");
		//scanf("%d", &n);
	}

	//  Get ending time and report
	end = clock();

	printf("Total write time %fs / dataset\n", (double)(end - begin) / CLOCKS_PER_SEC/(long long)numberDatasets);


	//  Complete
	zstructFree(ztransfer);
	free(data);

	//  Now read the same set
	//  Set start time
	begin = clock();

	ztransfer = zstructTransferNew((const char *)0, 0);
	for (i = 0; i<numberDatasets; i++) {
		sprintf(pathname, "/%s/Size %d/c/d/e/number %d/", apart, sizeOfRecord, i);
		ztransfer->pathname = pathname;
		ztransfer->pathnameLength = (int)strlen(pathname);
		ztransfer->values1 = (int *)readData;
		ztransfer->values1Mode = sizeOfRecord;  //  If read is not allocating space, this is the array size
		status = zread(ifltab, ztransfer);
		if (status != STATUS_OKAY) {
			printf("Error in read, status = %d\n", status);
			return status;
		}
	}


	//  Get ending time and report
	end = clock();
	printf("Total read time %fs / dataset\n", (double)(end - begin) / CLOCKS_PER_SEC / (long long)numberDatasets);


	zstructFree(ztransfer);
	free(readData);

	//  Finally, get a list of the pathnames in the file
	catStruct = zstructCatalogNew();

	//  Set start time
	begin = clock();

	//int zcatalog(long long *ifltab, char *pathWithWild, zStructCatalog *catStruct, int boolSorted);
	npaths = zcatalog(ifltab, (const char*)0, catStruct, 0);

	//  Get ending time and report
	end = clock();
	printf("Total catalog time = %fs  for %d paths\n", (double)(end - begin) / CLOCKS_PER_SEC ,numberDatasets);

	//for (i=0; i<npaths; i++) printf("%s\n", catStruct->pathnameList[i]);
	zstructFree(catStruct);
	zclose(ifltab);

	zcloseLog();

	printf("test complete.\n");

	return 0;

	
}
