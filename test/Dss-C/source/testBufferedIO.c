#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "hecdss7.h"
#include "zdssMessages.h"
#include "zStructTimeSeries.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "hecdssInternal.h"
#include "zdssLocking.h"
#include "TestDssC.h"




//  Test buffered IO in DSS-7
//
//  This function will call znewWrite and then store values so that
//  we can write different types of data to buffered IO 
//  for testing.

int testBufferedIO(long long *ifltab)
{
	float fvalues[1000];
	float dvalues[2000];
	float mdvalues[2000];
	int ivalues[1000];
	float rfvalues1[1000];
	float rfvalues2[1000];
	float rdvalues1[2000];
	float rdvalues2[2000];
	double dvalues1[2000];
	double dvalues2[2000];
	int ivals1[2000];
	int ivals2[2000];
	int rivalues1[1000];
	int rivalues2[1000];
	long long info[100];

	long long address;
	long long iadd;

	int numberInternalHeader;
	int numberCompressionHeader;
	int numberValues3;
	int numberUserHeader;
	int numberValues;
	int logicalNumberData;
	int dataType;
	int size;
	int buffSize;

	int v, nv;


	char mess[100];
	int i;
	int status;


	char pathname[] = "/a/b/c/d/e/buffer test/";

	int *buffer;
	long long bufferControl[4];
	bufferControl[BUFF_SIZE] = 0;
	bufferControl[BUFF_STAT] = 0;
	bufferControl[BUFF_ADDRESS] = 0;
	bufferControl[BUFF_INTS_USED] = 0;

	//  A version 7 function only
	if (zgetVersion(ifltab) != 7) return 0;

	printf("\n\n\nREDO THIS FUNCTION TO REALLY TEST BUFFERED IO\n\n\n\n");

	for (i=0; i<1000; i++) {
		fvalues[i] = (float)i;	
		ivalues[i] = i;
	}

	for (i=0; i<2000; i++) {
		dvalues[i] = (float)i;
		mdvalues[i] = -(float)i;
	}

	//  zcheck(long long *ifltab, const char* pathname, int statusWanted, int *istat)
	status = zcheck(ifltab, pathname);
	if (zisError(status)) {
		//  An error code 
		return status;
	}
	if (status == STATUS_RECORD_FOUND) {
		zdelete(ifltab, pathname);
	}

	buffSize = 3000;
	buffer = (int *)calloc(buffSize, 4);
	bufferControl[BUFF_SIZE] = buffSize;

	if (getEndian() && (zgetVersion(ifltab) == 7)) {
		zswitchInts(ivalues, 499);
		zswitchInts((int *)fvalues, 200);
	}

	status = zwritec (ifltab, pathname, 			
			 ivalues, 499,
			 ivalues, 205,
			 fvalues, 99,
			 fvalues, 200,
			 mdvalues, 2000, 
			 0, 0,
			 2000, 2000, 
			 0, 0, 0);
	
	if (zisError(status)) {
		stringCopy(mess, sizeof(mess), "Error zwriteInternal in testBufferedIO status Loc 1 ", sizeof(mess)); 
		i = 1;
		checknumbers_(&i, &status, mess, &status, strlen(mess));
		return status;
	}

	if (getEndian() && (zgetVersion(ifltab) == 7)) {
		zswitchInts(ivalues, 499);
		zswitchInts((int *)fvalues, 200);
	}

	status = zreadInfoBlock (ifltab, pathname, 0, info, 100, &i);
	if (status != STATUS_RECORD_FOUND) {
		stringCopy(mess, sizeof(mess), "Error reading zreadInfoBlock in testBufferedIO status Loc 2", sizeof(mess)); 
		i = 1;
		checknumbers_(&i, &status, mess, &status, strlen(mess));
		return status;
	}
	address = info[zdssInfoKeys.kinfoValues1Address];

	status = zreadc (ifltab, pathname,  
		 rivalues1, 1000, &numberInternalHeader,
		 rivalues2, 1000, &numberCompressionHeader,
		 (void *)rfvalues1, 1000, &numberValues3,
		 (void *)rfvalues2, 1000, &numberUserHeader,
		 (void *)rdvalues1, 2000, &numberValues,
		 &v, 0, &nv,
		 &nv, &logicalNumberData, 
		 &nv, &nv, &dataType);


	if (zisError(status)) {
		stringCopy(mess, sizeof(mess), "Error reading in testBufferedIO status Loc 3", sizeof(mess)); 
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

	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 10, numberInternalHeader ", sizeof(mess)); 
	i = 499;
	checknumbers_(&i, &numberInternalHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 10, numberCompressionHeader ", sizeof(mess)); 
	i = 205;
	checknumbers_(&i, &numberCompressionHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 10, numberValues3 ", sizeof(mess)); 
	i = 99;
	checknumbers_(&i, &numberValues3, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 10, numberUserHeader ", sizeof(mess)); 
	i = 200;
	checknumbers_(&i, &numberUserHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 10, numberValues ", sizeof(mess)); 
	i = 2000;
	checknumbers_(&i, &numberValues, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 10, logicalNumberData ", sizeof(mess)); 
	i = 2000;
	checknumbers_(&i, &logicalNumberData, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;


	i = 1;
	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 20, InternalHeader values ", sizeof(mess)); 
	checkints_(ivalues, rivalues1, &i, &numberInternalHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 20, compressionHeader values ", sizeof(mess)); 
	checkints_(ivalues, rivalues2, &i, &numberCompressionHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 20, Values3 values ", sizeof(mess)); 
	checkfloats_(fvalues, rfvalues1, &numberValues3, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 20, userHeader values ", sizeof(mess)); 
	checkfloats_(fvalues, rfvalues2, &numberUserHeader, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	i = 2000;
	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 20, data values ", sizeof(mess)); 
	checkfloats_(mdvalues, rdvalues1, &i, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	//printf ("\nAfter regular read.... now buffer IO testing\n\n");

	//  Now we have 2000 float values to test buffereing
/*
*		Actions
*		0 � No buffer use; write data directly to disk
*		1 - write this data to buffer
*		2 - write buffer and data to disk
*
*		BufferControl
*		bufferControl[0] is (max) int*4 size
*		bufferControl[1] is write status;  0 - unused, 1 - not dirty (read only), 2 - dirty (needs to write)
*		bufferControl[2] is file address
*		bufferControl[3] is current int number used
*
*
		int zputBuff (long long *ifltab, long long iaddress, int *iarray, int numberWords, int wordSize, 
			  int bufferAction, long long bufferControl[4], int *buffer)

		int zgetBuff (long long *ifltab, long long iaddress, int *iarray, int numberWords, int wordSize, 
			int bufferAction, long long bufferControl[4], int *buffer)



*/
	zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_ON, LOCKING_FLUSH_ON);
	for (i=0; i<2000; i++) {
		rdvalues1[i] = 0.0;
	}
	bufferControl[0] = 2000;
	bufferControl[1] = 0;
	//  Load the data into buffer
	status = zgetBuff(ifltab, address, (int *)rdvalues1, 2000, 1, BUFF_LOAD, bufferControl, buffer);
	if (status != STATUS_OKAY) {
		printf("Buffer read failed at location 40\n");
		return -1;
	}
	//  Copy from buffer into rdvalues1
	status = zgetBuff(ifltab, address, (int *)rdvalues1, 2000, 1, BUFF_READ, bufferControl, buffer);
	if (status != STATUS_OKAY) {
		printf("Buffer copy failed at location 40\n");
		return -1;
	}

	//  Check
	i = 2000;
	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 30, data values ", sizeof(mess)); 
	checkfloats_(mdvalues, rdvalues1, &i, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;
		
	//  Buffer now contains the correct data
	//  Lets mess with it
	//  write floats to disk and check get buff
	//ifltab[zdssKeys.kwritingNow] = 1;
	status = zputBuff(ifltab, address, (int *)rdvalues1, 2000, 1, BUFF_WRITE_FLUSH, bufferControl, buffer);
	if (status != STATUS_OKAY) {
		printf("zputBuff failed at location 40\n");
		return -1;
	}
	status = zgetBuff(ifltab, address, (int *)rdvalues2, 2000, 1, BUFF_READ, bufferControl, buffer);
	if (status != STATUS_OKAY) {
		printf("zgetBuff failed at location 41\n");
		return -1;
	}
	i = 2000;
	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 40, data values ", sizeof(mess)); 
	checkfloats_(rdvalues2, rdvalues1, &i, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	//  Change some floats and be sure buff has changed
	for (i=200; i<499; i++) {
		rdvalues2[i] = (float)((float)i + 2000.0);
	}
	iadd = address + (long long)(200/2);
	status = zputBuff(ifltab, iadd, (int *)&rdvalues2[200], 299, 1, BUFF_WRITE, bufferControl, buffer);
	if (status != STATUS_OKAY) {
		printf("zputBuff failed at location 50\n");
		return -1;
	}
	status = zgetBuff(ifltab, address, (int *)rdvalues1, 2000, 1, BUFF_READ, bufferControl, buffer);
		if (status != STATUS_OKAY) {
		printf("zgetBuff failed at location 50\n");
		return -1;
	}
	i = 2000;
	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 50, data values ", sizeof(mess)); 
	checkfloats_(rdvalues2, rdvalues1, &i, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	//  write doubles to buff (buffer should be able to contain anything)
	//  and be sure buff correctly has both floats and doubles
	for (i=0; i<1000; i++) {
		dvalues1[i] = -1.0;
		dvalues2[i] = -1.0;
	}
	for (i=600; i<899; i++) {
		dvalues1[i] = (double)i + 2000.0;
	}
	iadd = address + (long long)(600);
	status = zputBuff(ifltab, iadd, (int *)&dvalues1[600], 299, 2, BUFF_WRITE, bufferControl, buffer);
		if (status != STATUS_OKAY) {
		printf("zputBuff failed at location 60\n");
		return -1;
	}
	status = zgetBuff(ifltab, iadd, (int *)&dvalues2[600], 299, 2, BUFF_READ, bufferControl, buffer);
		if (status != STATUS_OKAY) {
		printf("zgetBuff failed at location 60\n");
		return -1;
	}
	i = 1000;
	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 60, data values ", sizeof(mess)); 
	checkdoubles_(dvalues1, dvalues2, &i, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	//  write combo buffer to disk, read disk and check still the same
	status = zputBuff(ifltab, iadd, (int *)&dvalues1[600], 299, 2, BUFF_WRITE_FLUSH, bufferControl, buffer);
	if (status != STATUS_OKAY) {
		printf("zputBuff failed at location 70\n");
		return -1;
	}
	status = zget(ifltab, iadd, (int *)&dvalues2[600], 299, 2);
	if (status != STATUS_OKAY) {
		printf("zget failed at location 70\n");
		return -1;
	}
	i = 1000;
	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 70, data values ", sizeof(mess)); 
	checkdoubles_(dvalues1, dvalues2, &i, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;


	//  Overwrite buffer and check that it is handled
	bufferControl[BUFF_SIZE] = 400;  //   wire to cause overflow
	status = zgetBuff(ifltab, address, (int *)rdvalues1, 400, 1, BUFF_LOAD, bufferControl, buffer);
	if (status != STATUS_OKAY) {
		printf("zgetBuff failed at location 80a\n");
		return -1;
	}
	status = zgetBuff(ifltab, address, (int *)rdvalues1, 400, 1, BUFF_READ, bufferControl, buffer);
	if (status != STATUS_OKAY) {
		printf("zgetBuff failed at location 80b\n");
		return -1;
	}
	i = 400;
	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 80, data values ", sizeof(mess)); 
	checkfloats_(rdvalues2, rdvalues1, &i, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = zget(ifltab, address, (int *)dvalues1, 1000, 2);
	if (status != STATUS_OKAY) {
		printf("zget failed at location 80\n");
		return -1;
	}

	for (i=200; i<350; i++) {
		dvalues1[i] = (double)i + 4000.0;
	}
	for (i=0; i<1000; i++) {
		dvalues2[i] = -1.0;
	}
	iadd = address + (long long)(200);
	//  Make the buffer dirty
	status = zputBuff(ifltab, address, (int *)dvalues1, 1, 2, BUFF_WRITE, bufferControl, buffer);
	if (status != STATUS_OKAY) {
		printf("zputBuff failed at location 90a\n");
		return -1;
	}
	status = zputBuff(ifltab, iadd, (int *)&dvalues1[200], 150, 2, BUFF_WRITE, bufferControl, buffer);
	if (status != STATUS_OKAY) {
		printf("zputBuff failed at location 90b\n");
		return -1;
	}
	status = zgetBuff(ifltab, address, ivals2, 2000, 1, BUFF_READ, bufferControl, buffer);
	if (status != STATUS_OKAY) {
		printf("zgetBuff failed at location 90c\n");
		return -1;
	}
	zget(ifltab, address, ivals1, 2000, 1);
	if (status != STATUS_OKAY) {
		printf("zget failed at location 90d\n");
		return -1;
	}
	i = 2000;
	size = 1;
	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 90, data values ", sizeof(mess)); 
	checkints_(ivals1, ivals2, &size, &i, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	zget(ifltab, address, (int *)dvalues2, 2000, 1);
	if (status != STATUS_OKAY) {
		printf("zget failed at location 100\n");
		return -1;
	}
	i = 1000;
	stringCopy(mess, sizeof(mess), "testBufferedIO Loc 100, data values ", sizeof(mess)); 
	checkdoubles_(dvalues1, dvalues2, &i, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	free(buffer);
	
	zlockActive(ifltab, LOCKING_LEVEL_LOW, LOCKING_LOCK_OFF, LOCKING_FLUSH_ON);
	printf("Completed Buffered IO test successfully!\n");

	return 0; 
}

