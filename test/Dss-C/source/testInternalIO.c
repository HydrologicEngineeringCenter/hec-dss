#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "hecdss7.h"
#include "zdssMessages.h"
#include "zStructTimeSeries.h"
#include "hecdssInternal.h"
#include "TestDssC.h"


//  Test simple write and read, 
//  without any buffer

int testInternalIO(long long *ifltab)
{
	float fvalues[1000];
	double dvalues[1000];
	int ivalues[1000];
	float rfvalues1[1000];
	float rfvalues2[1000];
	double rdvalues1[1000];
	int rivalues1[1000];
	int rivalues2[1000];

	int numberInternalHeader;
	int numberCompressionHeader;
	int numberValues3;
	int numberUserHeader;
	int numberValues;
	int logicalNumberData;
	int dataType;
	int version;
	int v, nv;
	char mess[100];
	int i;
	int status;
	char pathname[] = "/a/b/c/d/e/IO Test/";

	long long bufferControl[4];
	bufferControl[BUFF_SIZE] = 0;
	bufferControl[BUFF_STAT] = 0;
	bufferControl[BUFF_ADDRESS] = 0;
	bufferControl[BUFF_INTS_USED] = 0;


	//  Note, version 6 does not support multiple data / header areas.
	version = zgetVersion(ifltab);

	for (i=0; i<1000; i++) {
		fvalues[i] = (float)i;
		dvalues[i] = (double)i;		
		ivalues[i] = i;
	}

	//  zcheck(long long *ifltab, const char* pathname, int statusWanted, int *istat)
	//zsetMessageLevel(zmessaging_check_id, MESS_INTERNAL_DIAG_2);
	status = zcheck(ifltab, pathname);
	if ((status != STATUS_RECORD_FOUND) && (status != STATUS_RECORD_NOT_FOUND)) {
		//  An error code 
		return status;
	}
	if (status != STATUS_OKAY) {
		zdelete(ifltab, pathname);
	}

	if (getEndian() && (zgetVersion(ifltab) == 7)) {
		zswitchInts(ivalues, 499);
		zswitchInts((int *)fvalues, 200);
	}
	//zsetMessageLevel(zmessaging_write_id, MESS_INTERNAL_DIAG_2);
	//zsetMessageLevel(zmessaging_global_id, MESS_INTERNAL_DIAG_2);
	status = zwritec (ifltab, pathname,  			 
			 ivalues, 499,
			 ivalues, 205,
			 fvalues, 99,
			 fvalues, 200,
			 dvalues, 1998,
			 0, 0,
			 999, 999,  
			 1998, 0, 4);

	if (getEndian() && (zgetVersion(ifltab) == 7)) {
		zswitchInts(ivalues, 499);
		zswitchInts((int *)fvalues, 200);
	}


/*
int zreadc (long long *ifltab, const char* pathname, 			 
			 int *internalHeader, int internalHeaderArraySize , int *internalHeaderNumber,
			 int *header2, int header2ArraySize, int *header2Number,
			 int *values3, int values3ArraySize, int *values3Number,
			 int *userHeader, int userHeaderArraySize, int *userHeaderNumber,
			 int *values1, int values1Size, int *values1Number,
			 int *values2, int values2Size, int *values2Number,
			 int *numberValues, int *logicalNumberValues, 
			 int *totalAllocatedSize, int *totalExpandedSize, int *dataType)
*/

	status = zreadc (ifltab, pathname,  
		 rivalues1, 1000, &numberInternalHeader,
		 rivalues2, 1000, &numberCompressionHeader,
		 (void *)rfvalues1, 1000, &numberValues3,
		 (void *)rfvalues2, 1000, &numberUserHeader,
		 (void *)rdvalues1, 2000, &numberValues,
		 &v, 0, &nv,
		 &nv, &logicalNumberData, 
		 &nv, &nv, &dataType);


	if (status != STATUS_OKAY) {
		stringCopy(mess, sizeof(mess), "Error reading in testInternalIO status ", sizeof(mess)); 
		i = 0;
		checknumbers_(&i, &status, mess, &status, strlen(mess));
		return status;
	}

	if (getEndian() && (zgetVersion(ifltab) == 7)) {
		zswitchInts(rivalues1, numberInternalHeader);
		zswitchInts(rivalues2, numberCompressionHeader);
		zswitchInts((int *)rfvalues1, numberValues3);
		zswitchInts((int *)rfvalues2, numberUserHeader);
	}

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 10, numberInternalHeader ", sizeof(mess)); 
	i = 499;
	checknumbers_(&i, &numberInternalHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 10, numberCompressionHeader ", sizeof(mess)); 
	i = 205;
	checknumbers_(&i, &numberCompressionHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	if (version == 7) {
		stringCopy(mess, sizeof(mess), "testInternalIO Loc 10, numberValues3 ", sizeof(mess)); 
		i = 99;
		checknumbers_(&i, &numberValues3, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 10, numberUserHeader ", sizeof(mess)); 
	i = 200;
	checknumbers_(&i, &numberUserHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 10, numberValues ", sizeof(mess)); 
	i = 1998;
	checknumbers_(&i, &numberValues, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	if (version == 7) {
		stringCopy(mess, sizeof(mess), "testInternalIO Loc 10, logicalNumberData ", sizeof(mess)); 
		i = 999;
		checknumbers_(&i, &logicalNumberData, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 10, data type ", sizeof(mess)); 
	i = 4;
	checknumbers_(&i, &dataType, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;


	i = 1;
	stringCopy(mess, sizeof(mess), "testInternalIO Loc 20, internalHeader values ", sizeof(mess)); 
	checkints_(ivalues, rivalues1, &i, &numberInternalHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 20, compressionHeader values ", sizeof(mess)); 
	checkints_(ivalues, rivalues2, &i, &numberCompressionHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 20, Values3 values ", sizeof(mess)); 
	checkfloats_(fvalues, rfvalues1, &numberValues3, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 20, userHeader values ", sizeof(mess)); 
	checkfloats_(fvalues, rfvalues2, &numberUserHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	i = 999;
	stringCopy(mess, sizeof(mess), "testInternalIO Loc 20, data values ", sizeof(mess)); 
	checkdoubles_(dvalues, rdvalues1, &i, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;


	if (getEndian() && (zgetVersion(ifltab) == 7)) {
		zswitchInts(ivalues, 499);
		zswitchInts((int *)fvalues, 200);
	}

	status = zwritec (ifltab, pathname, 			
			 ivalues, 499,
			 ivalues, 205,
			 fvalues, 99,
			 fvalues, 200,
			 dvalues, 1996,
			 0, 0,
			 998, 998,  
			 1996, 0, 4);

	if (getEndian() && (zgetVersion(ifltab) == 7)) {
		zswitchInts(ivalues, 499);
		zswitchInts((int *)fvalues, 200);
	}



	status = zreadc (ifltab, pathname,  
		 rivalues1, 1000, &numberInternalHeader,
		 rivalues2, 1000, &numberCompressionHeader,
		 (void *)rfvalues1, 1000, &numberValues3,
		 (void *)rfvalues2, 1000, &numberUserHeader,
		 (void *)rdvalues1, 2000, &numberValues,
		 &v, 0, &nv,
		 &nv, &logicalNumberData, 
		 &nv, &nv, &dataType);

	if (status != STATUS_OKAY) {
		stringCopy(mess, sizeof(mess), "Error reading in testInternalIO status ", sizeof(mess)); 
		i = 0;
		checknumbers_(&i, &status, mess, &status, strlen(mess));
		return status;
	}

	if (getEndian() && (zgetVersion(ifltab) == 7)) {
		zswitchInts(rivalues1, numberInternalHeader);
		zswitchInts(rivalues2, numberCompressionHeader);
		zswitchInts((int *)rfvalues1, numberValues3);
		zswitchInts((int *)rfvalues2, numberUserHeader);
	}

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 110, numberInternalHeader ", sizeof(mess)); 
	i = 499;
	checknumbers_(&i, &numberInternalHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 110, numberCompressionHeader ", sizeof(mess)); 
	i = 205;
	checknumbers_(&i, &numberCompressionHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	if (version == 7) {
		stringCopy(mess, sizeof(mess), "testInternalIO Loc 110, numberValues3 ", sizeof(mess)); 
		i = 99;
		checknumbers_(&i, &numberValues3, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 110, numberUserHeader ", sizeof(mess)); 
	i = 200;
	checknumbers_(&i, &numberUserHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 110, numberValues ", sizeof(mess)); 
	i = 1996;
	checknumbers_(&i, &numberValues, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	if (version == 7) {
		stringCopy(mess, sizeof(mess), "testInternalIO Loc 110, logicalNumberData ", sizeof(mess)); 
		i = 998;
		checknumbers_(&i, &logicalNumberData, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 110, data type ", sizeof(mess)); 
	i = 4;
	checknumbers_(&i, &dataType, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;


	i = 1;
	stringCopy(mess, sizeof(mess), "testInternalIO Loc 120, internalHeader values ", sizeof(mess)); 
	checkints_(ivalues, rivalues1, &i, &numberInternalHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 120, compressionHeader values ", sizeof(mess)); 
	checkints_(ivalues, rivalues2, &i, &numberCompressionHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 120, Values3 values ", sizeof(mess)); 
	checkfloats_(fvalues, rfvalues1, &numberValues3, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 120, userHeader values ", sizeof(mess)); 
	checkfloats_(fvalues, rfvalues2, &numberUserHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	i = 998;
	stringCopy(mess, sizeof(mess), "testInternalIO Loc 120, data values ", sizeof(mess)); 
	checkdoubles_(dvalues, rdvalues1, &i, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	if (getEndian() && (zgetVersion(ifltab) == 7)) {
		zswitchInts(ivalues, 499);
		zswitchInts((int *)fvalues, 200);
	}

		status = zwritec (ifltab, pathname,			
			 ivalues, 499,
			 ivalues, 205,
			 fvalues, 99,
			 fvalues, 200,
			 dvalues, 2000,
			 0, 0,
			 1000, 1000,  
			 2000, 0, 4);

		if (getEndian() && (zgetVersion(ifltab) == 7)) {
			zswitchInts(ivalues, 499);
			zswitchInts((int *)fvalues, 200);
		}

	status = zreadc (ifltab, pathname,  
		 rivalues1, 1000, &numberInternalHeader,
		 rivalues2, 1000, &numberCompressionHeader,
		 (void *)rfvalues1, 1000, &numberValues3,
		 (void *)rfvalues2, 1000, &numberUserHeader,
		 (void *)rdvalues1, 2000, &numberValues,
		 &v, 0, &nv,
		 &nv, &logicalNumberData, 
		 &nv, &nv, &dataType);

	if (status != STATUS_OKAY) {
		stringCopy(mess, sizeof(mess), "Error reading in testInternalIO status ", sizeof(mess)); 
		i = 0;
		checknumbers_(&i, &status, mess, &status, strlen(mess));
		return status;
	}

	if (getEndian() && (zgetVersion(ifltab) == 7)) {
		zswitchInts(rivalues1, numberInternalHeader);
		zswitchInts(rivalues2, numberCompressionHeader);
		zswitchInts((int *)rfvalues1, numberValues3);
		zswitchInts((int *)rfvalues2, numberUserHeader);
	}

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 210, numberInternalHeader ", sizeof(mess)); 
	i = 499;
	checknumbers_(&i, &numberInternalHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 210, numberCompressionHeader ", sizeof(mess)); 
	i = 205;
	checknumbers_(&i, &numberCompressionHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	if (version == 7) {
		stringCopy(mess, sizeof(mess), "testInternalIO Loc 210, numberValues3 ", sizeof(mess)); 
		i = 99;
		checknumbers_(&i, &numberValues3, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 210, numberUserHeader ", sizeof(mess)); 
	i = 200;
	checknumbers_(&i, &numberUserHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 210, numberValues ", sizeof(mess)); 
	i = 2000;
	checknumbers_(&i, &numberValues, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	if (version == 7) {
		stringCopy(mess, sizeof(mess), "testInternalIO Loc 210, logicalNumberData ", sizeof(mess)); 
		i = 1000;
		checknumbers_(&i, &logicalNumberData, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 210, data type ", sizeof(mess)); 
	i = 4;
	checknumbers_(&i, &dataType, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;


	i = 1;
	stringCopy(mess, sizeof(mess), "testInternalIO Loc 220, internalHeader values ", sizeof(mess)); 
	checkints_(ivalues, rivalues1, &i, &numberInternalHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 220, compressionHeader values ", sizeof(mess)); 
	checkints_(ivalues, rivalues2, &i, &numberCompressionHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 220, Values3 values ", sizeof(mess)); 
	checkfloats_(fvalues, rfvalues1, &numberValues3, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testInternalIO Loc 220, userHeader values ", sizeof(mess)); 
	checkfloats_(fvalues, rfvalues2, &numberUserHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	i = 999;
	stringCopy(mess, sizeof(mess), "testInternalIO Loc 220, data values ", sizeof(mess)); 
	checkdoubles_(dvalues, rdvalues1, &i, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;


	printf("Completed Internal IO test successfully!\n");

	return 0; 
}

