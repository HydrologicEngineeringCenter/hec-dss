#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "hecdssInternal.h"
#include "zStructTimeSeries.h"
#include "TestDssC.h"
#include "TestDssC.h"



/*  

	This will do a basic test of the following functions
	
		zwrite, zread
		zwritea, zreada
		zwritex, zreadx
		zwritec, zreadc

	for both DSS Version 7 and Version 6 files.
	Note:  Version 6 cannot store all the
	arguments version 7 has, so only a subset can be tested
	

	It will not test expansions, deleting, etc.
	It will test for basic errors, such as calling
	a function with the wrong version 
	(i.e., testing the testing for errors)

*/

void setTransferStruct(zStructTransfer *ztransfer1,
						float *fvalues,
						double *dvalues,
						int *ivalues1,
						int *ivalues2,
						int *ivalues3,
						int *ivalues4,
						int *ivalues5);

void allocateTransfereStruct(zStructTransfer *zStructTransfer);
zStructTransfer *cloneTransfereStruct(zStructTransfer *ztransfer1);


int testIO_Interface(long long *ifltab7, long long *ifltab6)
{

	char pathname[MAX_PATHNAME_LENGTH];	
	
	int status;
	int i;
	int len;
	int plan;
	int recordFound;

	zStructTransfer *ztransfer1;
	zStructTransfer *ztransfer2;
	zStructTransfer *ztransfer3;
	zStructTransfer *ztransfer4;

	float fvalues[1000];
	double dvalues[1000];
	int ivalues1[1000];
	int ivalues2[1000];
	int ivalues3[1000];
	int ivalues4[1000];
	int ivalues5[1000];

	//  Test collections path processing
	len = copyAndTrim (pathname, MAX_PATHNAME_LENGTH, "/A/B/C/D/E/c:abc|f part/", MAX_PATHNAME_LENGTH-5);
	status = zcollectionsPath(pathname, len);
	if (status != 1) {
		printf("Failed zcollectionsPath in testIO_Interface, loc 1!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}

	len = copyAndTrim (pathname, MAX_PATHNAME_LENGTH, "//////c:12345678901234567890|f part/", MAX_PATHNAME_LENGTH-5);
	status = zcollectionsPath(pathname, len);
	if (status != 1) {
		printf("Failed zcollectionsPath in testIO_Interface, loc 2!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}

	len = copyAndTrim (pathname, MAX_PATHNAME_LENGTH, "//////c;12345678901234567890|f part/", MAX_PATHNAME_LENGTH-5);
	status = zcollectionsPath(pathname, len);
	if (status != 0) {
		printf("Failed zcollectionsPath in testIO_Interface, loc 3!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}



	for (i=0; i<1000; i++) {
		fvalues[i] = (float)i;
		dvalues[i] = (double)i;		
		ivalues1[i] = i;
		ivalues2[i] = -i;
		ivalues3[i] = (i * 100) + i;
		ivalues4[i] = (i * 100) - i;
		ivalues5[i] = (i * 2);
	}
	//zsetMessageGroupLevel("read", 4);

	////////////////////////////////////
	/////////////  test zread, zwrite
	////////////////////////////////////

	copyAndTrim (pathname, MAX_PATHNAME_LENGTH, "/Test IO Interface/B/C/D/E/zwrite7 - zread7/", MAX_PATHNAME_LENGTH-5);

	ztransfer1 = zstructTransferNew(pathname, 0);
	if (!ztransfer1) {
		printf("Failed to create zStructTransfer in testIO_Interface, Loc 1!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}

	setTransferStruct(ztransfer1,
		fvalues, dvalues, ivalues1, ivalues2,
		ivalues3, ivalues4, ivalues5);

	ztransfer1->totalAllocatedSize = 100000;
	//zset("MLVL", "", 15);
	if (getEndian() && (zgetVersion(ifltab7) == 7)) {
		zswitchInts(ztransfer1->values1, 1000);
		zswitchInts(ztransfer1->internalHeader, 1000);
		zswitchInts(ztransfer1->header2, 1000);
		zswitchInts(ztransfer1->values3, 1000);
		zswitchInts(ztransfer1->userHeader, 1000);
	}
	status =  zwrite(ifltab7, ztransfer1);
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zwrite, Loc 2")) return status;
	if (getEndian() && (zgetVersion(ifltab7) == 7)) {
		zswitchInts(ztransfer1->values1, 1000);
		zswitchInts(ztransfer1->internalHeader, 1000);
		zswitchInts(ztransfer1->header2, 1000);
		zswitchInts(ztransfer1->values3, 1000);
		zswitchInts(ztransfer1->userHeader, 1000);
	}

	ztransfer2 = zstructTransferNew(pathname, 1);
	if (!ztransfer2) {
		printf("Failed to create ztransfer2 in testIO_Interface, Loc 3!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}

	status =  zread(ifltab7, ztransfer2);
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zread, Loc 4")) return status;

	if (getEndian() && (zgetVersion(ifltab7) == 7)) {
		zswitchInts(ztransfer2->values1, ztransfer2->values1Number);
		zswitchInts(ztransfer2->internalHeader, ztransfer2->internalHeaderNumber);
		zswitchInts(ztransfer2->header2, ztransfer2->header2Number);
		zswitchInts(ztransfer2->values3, ztransfer2->values3Number);
		zswitchInts(ztransfer2->userHeader, ztransfer2->userHeaderNumber);
	}
	status = zcompareDataSets(ifltab7, ztransfer1, ztransfer2,  1, 0, ztransfer1->pathname, "Fail in testIO_Interface, Location 5");
	if (status) return status;

	zstructFree(ztransfer1);
	zstructFree(ztransfer2);



	//  Now with version 6
	copyAndTrim (pathname, MAX_PATHNAME_LENGTH, "/Test IO Interface/B/C/D/E/zwrite6 - zread6/", MAX_PATHNAME_LENGTH-5);

	ztransfer1 = zstructTransferNew(pathname, 0);
	if (!ztransfer1) {
		printf("Failed to create zStructTransfer in testIO_Interface, Loc 10!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}
	setTransferStruct(ztransfer1,
		fvalues, dvalues, ivalues1, ivalues2,
		ivalues3, ivalues4, ivalues5);

	//  Don't need to swap ints, since vers 6 is 32 bit based.
	status =  zwrite(ifltab6, ztransfer1);
	if (zcheckStatus(ifltab6, status, 1, "Fail in testIO_Interface, zwrite 6, Loc 11")) return status;

	ztransfer2 = zstructTransferNew(pathname, 1);
	if (!ztransfer2) {
		printf("Failed to create ztransfer2 in testIO_Interface!, Loc 12\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}
	status =  zread(ifltab6, ztransfer2);

	
	if (zcheckStatus(ifltab6, status, 1, "Fail in testIO_Interface, zread 6, Loc 13")) return status;
	//  Version 6 cannot read new data types added to version 7, so clear those from the original struct
	ztransfer3 = cloneTransfereStruct(ztransfer1);
	ztransfer3->values3 = 0;
	ztransfer3->values3Number = 0;
	ztransfer3->values2 = 0;
	ztransfer3->values2Number = 0;
	ztransfer3->numberValues = 0;
	ztransfer3->logicalNumberValues = 0;
	ztransfer3->totalExpandedSize = 0;
	ztransfer3->totalAllocatedSize = 0;
	status = zcompareDataSets(ifltab6, ztransfer3, ztransfer2,  1, 0, ztransfer3->pathname, "Fail in testIO_Interface, Location 14");
	if (status) return status;

	zstructFree(ztransfer1);
	zstructFree(ztransfer2);
	zstructFree(ztransfer3);

	

	////////////////////////////////////
	/////////////  test zreadc, zwritec
	////////////////////////////////////

	//  Now, call zwritec, essentially the same function, except using arguments instead of a struct
	//  Use the struct to pass arguments, so we can have an easier comparison

	copyAndTrim (pathname, MAX_PATHNAME_LENGTH, "/Test IO Interface/B/C/D/E/zwritec - zreadc/", MAX_PATHNAME_LENGTH-5);

	ztransfer1 = zstructTransferNew(pathname, 0);
	if (!ztransfer1) {
		printf("Failed to create zStructTransfer in testIO_Interface, Loc 20!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}
	setTransferStruct(ztransfer1,
		fvalues, dvalues, ivalues1, ivalues2,
		ivalues3, ivalues4, ivalues5);

	if (getEndian() && (zgetVersion(ifltab7) == 7)) {
		zswitchInts(ztransfer1->values1, 1000);
		zswitchInts(ztransfer1->internalHeader, 1000);
		zswitchInts(ztransfer1->header2, 1000);
		zswitchInts(ztransfer1->values3, 1000);
		zswitchInts(ztransfer1->userHeader, 1000);
	}
	
	status = zwritec(ifltab7, pathname, 
			ztransfer1->internalHeader, ztransfer1->internalHeaderNumber,
			ztransfer1->header2, ztransfer1->header2Number,
			ztransfer1->values3, ztransfer1->values3Number,
			ztransfer1->userHeader, ztransfer1->userHeaderNumber,
			ztransfer1->values1, ztransfer1->values1Number,
			ztransfer1->values2, ztransfer1->values2Number,
			ztransfer1->numberValues, ztransfer1->logicalNumberValues, 
			ztransfer1->totalAllocatedSize, ztransfer1->totalExpandedSize, 
			ztransfer1->dataType);
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zwritec, Loc 21")) return status;
	if (getEndian() && (zgetVersion(ifltab7) == 7)) {
		zswitchInts(ztransfer1->values1, 1000);
		zswitchInts(ztransfer1->internalHeader, 1000);
		zswitchInts(ztransfer1->header2, 1000);
		zswitchInts(ztransfer1->values3, 1000);
		zswitchInts(ztransfer1->userHeader, 1000);
	}

	ztransfer2 = zstructTransferNew(pathname, 1);
	if (!ztransfer2) {
		printf("Failed to create ztransfer2 in testIO_Interface, Loc 22!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}

	status =  zread(ifltab7, ztransfer2);
	if (getEndian() && (zgetVersion(ifltab7) == 7)) {
		zswitchInts(ztransfer2->values1, ztransfer2->values1Number);
		zswitchInts(ztransfer2->internalHeader, ztransfer2->internalHeaderNumber);
		zswitchInts(ztransfer2->header2, ztransfer2->header2Number);
		zswitchInts(ztransfer2->values3, ztransfer2->values3Number);
		zswitchInts(ztransfer2->userHeader, ztransfer2->userHeaderNumber);
	}
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zreadc, Loc 23")) return status;
	status = zcompareDataSets(ifltab7, ztransfer1, ztransfer2,  1, 0, ztransfer1->pathname, "Fail in testIO_Interface, Location 24");
	if (status) return status;

	zstructFree(ztransfer2);

	ztransfer2 = zstructTransferNew(pathname, 0);
	if (!ztransfer2) {
		printf("Failed to create ztransfer2 in testIO_Interface, Loc 25!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}

	allocateTransfereStruct(ztransfer2);
	status = zreadc (ifltab7, pathname, 			 
			 ztransfer2->internalHeader, ztransfer2->internalHeaderMode , &ztransfer2->internalHeaderNumber,
			 ztransfer2->header2, ztransfer2->header2Mode, &ztransfer2->header2Number,
			 ztransfer2->values3, ztransfer2->values3Mode, &ztransfer2->values3Number,
			 ztransfer2->userHeader, ztransfer2->userHeaderMode, &ztransfer2->userHeaderNumber,
			 ztransfer2->values1, ztransfer2->values1Mode, &ztransfer2->values1Number,
			 ztransfer2->values2, ztransfer2->values2Mode, &ztransfer2->values2Number,
			 &ztransfer2->numberValues, &ztransfer2->logicalNumberValues, 
			 &ztransfer2->totalAllocatedSize, &ztransfer2->totalExpandedSize,
			 &ztransfer2->dataType);
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zreadc, Loc 26")) return status;

	if (getEndian() && (zgetVersion(ifltab7) == 7)) {
		zswitchInts(ztransfer2->values1, ztransfer2->values1Number);
		zswitchInts(ztransfer2->internalHeader, ztransfer2->internalHeaderNumber);
		zswitchInts(ztransfer2->header2, ztransfer2->header2Number);
		zswitchInts(ztransfer2->values3, ztransfer2->values3Number);
		zswitchInts(ztransfer2->userHeader, ztransfer2->userHeaderNumber);
	}

	status = zcompareDataSets(ifltab7, ztransfer1, ztransfer2,  1, 0, ztransfer1->pathname, "Fail in testIO_Interface, Location 27");
	if (status) return status;
	
	zstructFree(ztransfer1);
	zstructFree(ztransfer2);


	//  Now with version 6
	copyAndTrim (pathname, MAX_PATHNAME_LENGTH, "/Test IO Interface/B/C/D/E/zwritec6 - zreadc6/", MAX_PATHNAME_LENGTH-5);

	ztransfer1 = zstructTransferNew(pathname, 0);
	if (!ztransfer1) {
		printf("Failed to create zStructTransfer in testIO_Interface, Loc 30!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}
	setTransferStruct(ztransfer1,
		fvalues, dvalues, ivalues1, ivalues2,
		ivalues3, ivalues4, ivalues5);
	
	status = zwritec(ifltab6, pathname, 
			ztransfer1->internalHeader, ztransfer1->internalHeaderNumber,
			ztransfer1->header2, ztransfer1->header2Number,
			ztransfer1->values3, ztransfer1->values3Number,
			ztransfer1->userHeader, ztransfer1->userHeaderNumber,
			ztransfer1->values1, ztransfer1->values1Number,
			ztransfer1->values2, ztransfer1->values2Number,
			ztransfer1->numberValues, ztransfer1->logicalNumberValues, 
			ztransfer1->totalAllocatedSize, ztransfer1->totalExpandedSize, 
			ztransfer1->dataType);
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zwritec, Loc 31")) return status;

	ztransfer2 = zstructTransferNew(pathname, 1);
	if (!ztransfer2) {
		printf("Failed to create ztransfer2 in testIO_Interface, Loc 32!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}

	status =  zread(ifltab6, ztransfer2);	
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zreadc, Loc 33")) return status;

	ztransfer3 = cloneTransfereStruct(ztransfer1);
	ztransfer3->values3 = 0;
	ztransfer3->values3Number = 0;
	ztransfer3->values2 = 0;
	ztransfer3->values2Number = 0;
	ztransfer3->numberValues = 0;
	ztransfer3->logicalNumberValues = 0;
	ztransfer3->totalExpandedSize = 0;
	ztransfer3->totalAllocatedSize = 0;
	//  zread reports back zero for vers 6 items, but does not change addresses
	ztransfer4 = cloneTransfereStruct(ztransfer2);
	ztransfer4->values3 = 0;
	ztransfer4->values2 = 0;
	status = zcompareDataSets(ifltab7, ztransfer3, ztransfer4,  1, 0, ztransfer3->pathname, "Fail in testIO_Interface, Loc 34");
	if (status) return status;

	zstructFree(ztransfer2);
	zstructFree(ztransfer3);
	zstructFree(ztransfer4);

	ztransfer2 = zstructTransferNew(pathname, 0);
	if (!ztransfer2) {
		printf("Failed to create ztransfer2 in testIO_Interface, Loc 35!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}

	allocateTransfereStruct(ztransfer2);
	status = zreadc (ifltab6, pathname, 			 
			 ztransfer2->internalHeader, ztransfer2->internalHeaderMode , &ztransfer2->internalHeaderNumber,
			 ztransfer2->header2, ztransfer2->header2Mode, &ztransfer2->header2Number,
			 ztransfer2->values3, ztransfer2->values3Mode, &ztransfer2->values3Number,
			 ztransfer2->userHeader, ztransfer2->userHeaderMode, &ztransfer2->userHeaderNumber,
			 ztransfer2->values1, ztransfer2->values1Mode, &ztransfer2->values1Number,
			 ztransfer2->values2, ztransfer2->values2Mode, &ztransfer2->values2Number,
			 &ztransfer2->numberValues, &ztransfer2->logicalNumberValues, 
			 &ztransfer2->totalAllocatedSize, &ztransfer2->totalExpandedSize,
			 &ztransfer2->dataType);
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zreadc, Loc 36")) return status;

	ztransfer3 = cloneTransfereStruct(ztransfer1);
	ztransfer3->values3 = 0;
	ztransfer3->values3Number = 0;
	ztransfer3->values2 = 0;
	ztransfer3->values2Number = 0;
	ztransfer3->numberValues = 0;
	ztransfer3->logicalNumberValues = 0;
	ztransfer3->totalExpandedSize = 0;
	ztransfer3->totalAllocatedSize = 0;//  zread reports back zero for vers 6 items, but does not change addresses
	ztransfer4 = cloneTransfereStruct(ztransfer2);
	ztransfer4->values3 = 0;
	ztransfer4->values2 = 0;
	status = zcompareDataSets(ifltab7, ztransfer3, ztransfer4,  1, 0, ztransfer3->pathname, "Fail in testIO_Interface, Loc 37");
	if (status) return status;

	zstructFree(ztransfer1);
	zstructFree(ztransfer2);
	zstructFree(ztransfer3);
	zstructFree(ztransfer4);


	////////////////////////////////////
	/////////////  test zreadx, zwritex
	////////////////////////////////////

	copyAndTrim (pathname, MAX_PATHNAME_LENGTH, "/Test IO Interface/B/C/D/E/zwritex - zreadx/", MAX_PATHNAME_LENGTH-5);

	ztransfer1 = zstructTransferNew(pathname, 0);
	if (!ztransfer1) {
		printf("Failed to create zStructTransfer in testIO_Interface, Loc 40!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}
	setTransferStruct(ztransfer1,
		fvalues, dvalues, ivalues1, ivalues2,
		ivalues3, ivalues4, ivalues5);

	len = (int)strlen(pathname);
	plan = 0;
	if (getEndian() && (zgetVersion(ifltab7) == 7)) {
		zswitchInts(ztransfer1->values1, 1000);
		zswitchInts(ztransfer1->internalHeader, 1000);
		zswitchInts(ztransfer1->header2, 1000);
		zswitchInts(ztransfer1->userHeader, 1000);
	}
	zwritex(ifltab7, pathname, &len,
			ztransfer1->internalHeader, &ztransfer1->internalHeaderNumber,
			ztransfer1->header2, &ztransfer1->header2Number,			
			ztransfer1->userHeader, &ztransfer1->userHeaderNumber,
			ztransfer1->values1, &ztransfer1->values1Number,						
			&ztransfer1->dataType, &plan, 
			&status, &recordFound);
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zwritex, Loc 41")) return status;
	if (getEndian() && (zgetVersion(ifltab7) == 7)) {
		zswitchInts(ztransfer1->values1, 1000);
		zswitchInts(ztransfer1->internalHeader, 1000);
		zswitchInts(ztransfer1->header2, 1000);
		zswitchInts(ztransfer1->userHeader, 1000);
	}
	ztransfer2 = zstructTransferNew(pathname, 1);
	if (!ztransfer2) {
		printf("Failed to create ztransfer2 in testIO_Interface, Loc 42!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}
	status =  zread(ifltab7, ztransfer2);
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zreadc, Loc 43")) return status;
	if (getEndian() && (zgetVersion(ifltab7) == 7)) {
		zswitchInts(ztransfer2->values1, ztransfer2->values1Number);
		zswitchInts(ztransfer2->internalHeader, ztransfer2->internalHeaderNumber);
		zswitchInts(ztransfer2->header2, ztransfer2->header2Number);
		zswitchInts(ztransfer2->userHeader, ztransfer2->userHeaderNumber);
	}
	//  remove the incompatible version 6 items
	ztransfer3 = cloneTransfereStruct(ztransfer1);
	ztransfer3->values3 = 0;
	ztransfer3->values3Number = 0;
	ztransfer3->values2 = 0;
	ztransfer3->values2Number = 0;
	ztransfer3->numberValues = 0;
	ztransfer3->logicalNumberValues = 0;
	ztransfer3->totalExpandedSize = 0;
	ztransfer3->totalAllocatedSize = 0;
	ztransfer4 = cloneTransfereStruct(ztransfer2);
	ztransfer4->values3 = 0;
	ztransfer4->values2 = 0;
	status = zcompareDataSets(ifltab7, ztransfer3, ztransfer4,  1, 0, ztransfer3->pathname, "Fail in testIO_Interface, Loc 44");
	if (status) return status;
	zstructFree(ztransfer2);
	zstructFree(ztransfer3);
	zstructFree(ztransfer4);

	ztransfer2 = zstructTransferNew(pathname, 0);
	if (!ztransfer2) {
		printf("Failed to create ztransfer2 in testIO_Interface, Loc 45!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}
	allocateTransfereStruct(ztransfer2);
	zreadx (ifltab7, pathname, 			 
			 ztransfer2->internalHeader, &ztransfer2->internalHeaderMode , &ztransfer2->internalHeaderNumber,
			 ztransfer2->header2, &ztransfer2->header2Mode, &ztransfer2->header2Number,			 
			 ztransfer2->userHeader, &ztransfer2->userHeaderMode, &ztransfer2->userHeaderNumber,
			 ztransfer2->values1, &ztransfer2->values1Mode, &ztransfer2->values1Number,
			 &plan, &recordFound);
	status = -1;
	if (recordFound) status = 0;
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zreadc, Loc 46")) return status;
	if (getEndian() && (zgetVersion(ifltab7) == 7)) {
		zswitchInts(ztransfer2->values1, ztransfer2->values1Number);
		zswitchInts(ztransfer2->internalHeader, ztransfer2->internalHeaderNumber);
		zswitchInts(ztransfer2->header2, ztransfer2->header2Number);
		zswitchInts(ztransfer2->userHeader, ztransfer2->userHeaderNumber);
	}

	ztransfer3 = cloneTransfereStruct(ztransfer1);
	ztransfer3->values3 = 0;
	ztransfer3->values3Number = 0;
	ztransfer3->values2 = 0;
	ztransfer3->values2Number = 0;
	ztransfer3->numberValues = 0;
	ztransfer3->logicalNumberValues = 0;
	ztransfer3->totalExpandedSize = 0;
	ztransfer3->totalAllocatedSize = 0;
	ztransfer3->dataType = 0;
	ztransfer4 = cloneTransfereStruct(ztransfer2);
	ztransfer4->values3 = 0;
	ztransfer4->values3Number = 0;
	ztransfer4->values2 = 0;
	ztransfer4->values2Number = 0;
	status = zcompareDataSets(ifltab7, ztransfer3, ztransfer4,  1, 0, ztransfer3->pathname, "Fail in testIO_Interface, Loc 47");
	if (status) return status;

	zstructFree(ztransfer1);
	zstructFree(ztransfer2);
	zstructFree(ztransfer3);
	zstructFree(ztransfer4);


	//  Now with version 6
	copyAndTrim (pathname, MAX_PATHNAME_LENGTH, "/Test IO Interface/B/C/D/E/zwritex6 - zreadx6/", MAX_PATHNAME_LENGTH-5);

	ztransfer1 = zstructTransferNew(pathname, 0);
	if (!ztransfer1) {
		printf("Failed to create zStructTransfer in testIO_Interface, Loc 50!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}
	setTransferStruct(ztransfer1,
		fvalues, dvalues, ivalues1, ivalues2,
		ivalues3, ivalues4, ivalues5);

	len = (int)strlen(pathname);
	plan = 0;
	zwritex(ifltab6, pathname, &len,
			ztransfer1->internalHeader, &ztransfer1->internalHeaderNumber,
			ztransfer1->header2, &ztransfer1->header2Number,			
			ztransfer1->userHeader, &ztransfer1->userHeaderNumber,
			ztransfer1->values1, &ztransfer1->values1Number,						
			&ztransfer1->dataType, &plan, 
			&status, &recordFound);
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zwritec, Loc 51")) return status;

	ztransfer2 = zstructTransferNew(pathname, 1);
	if (!ztransfer2) {
		printf("Failed to create ztransfer2 in testIO_Interface, Loc 52!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}

	status =  zread(ifltab6, ztransfer2);

	ztransfer3 = cloneTransfereStruct(ztransfer1);
	ztransfer3->values3 = 0;
	ztransfer3->values3Number = 0;
	ztransfer3->values2 = 0;
	ztransfer3->values2Number = 0;
	ztransfer3->numberValues = 0;
	ztransfer3->logicalNumberValues = 0;
	ztransfer3->totalExpandedSize = 0;
	ztransfer3->totalAllocatedSize = 0;
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zreadc, Loc 53")) return status;
	status = zcompareDataSets(ifltab7, ztransfer3, ztransfer2,  1, 0, ztransfer3->pathname, "Fail in testIO_Interface, Loc 54");
	if (status) return status;
	
	zstructFree(ztransfer2);
	zstructFree(ztransfer3);

	ztransfer2 = zstructTransferNew(pathname, 0);
	if (!ztransfer2) {
		printf("Failed to create ztransfer2 in testIO_Interface, Loc 55!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}

	allocateTransfereStruct(ztransfer2);
	zreadx (ifltab6, pathname, 			 
			 ztransfer2->internalHeader, &ztransfer2->internalHeaderMode , &ztransfer2->internalHeaderNumber,
			 ztransfer2->header2, &ztransfer2->header2Mode, &ztransfer2->header2Number,			 
			 ztransfer2->userHeader, &ztransfer2->userHeaderMode, &ztransfer2->userHeaderNumber,
			 ztransfer2->values1, &ztransfer2->values1Mode, &ztransfer2->values1Number,
			 &plan, &recordFound);
	status = -1;
	if (recordFound) status = 0;
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zreadc, Loc 56")) return status;

	ztransfer3 = cloneTransfereStruct(ztransfer1);
	ztransfer3->values3 = 0;
	ztransfer3->values3Number = 0;
	ztransfer3->values2 = 0;
	ztransfer3->values2Number = 0;
	ztransfer3->numberValues = 0;
	ztransfer3->logicalNumberValues = 0;
	ztransfer3->totalExpandedSize = 0;
	ztransfer3->totalAllocatedSize = 0;
	ztransfer3->dataType = 0;
	ztransfer4 = cloneTransfereStruct(ztransfer2);
	ztransfer4->values3 = 0;
	ztransfer4->values3Number = 0;
	ztransfer4->values2 = 0;
	ztransfer4->values2Number = 0;
	status = zcompareDataSets(ifltab7, ztransfer3, ztransfer4,  1, 0, ztransfer3->pathname, "Fail in testIO_Interface, Loc 57");
	if (status) return status;
	
	zstructFree(ztransfer1);
	zstructFree(ztransfer2);
	zstructFree(ztransfer3);
	zstructFree(ztransfer4);




	
	////////////////////////////////////
	/////////////  test zreada, zwritea
	////////////////////////////////////

	copyAndTrim (pathname, MAX_PATHNAME_LENGTH, "/Test IO Interface/B/C/D/E/zwritea - zreada/", MAX_PATHNAME_LENGTH-5);

	ztransfer1 = zstructTransferNew(pathname, 0);
	if (!ztransfer1) {
		printf("Failed to create zStructTransfer in testIO_Interface, Loc 60!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}
	setTransferStruct(ztransfer1,
		fvalues, dvalues, ivalues1, ivalues2,
		ivalues3, ivalues4, ivalues5);
	
	len = (int)strlen(pathname);
	plan = 0;
	if (getEndian() && (zgetVersion(ifltab7) == 7)) {
		zswitchInts(ztransfer1->values1, 1000);
		zswitchInts(ztransfer1->internalHeader, 1000);
		zswitchInts(ztransfer1->header2, 1000);
		zswitchInts(ztransfer1->values3, 1000);
		zswitchInts(ztransfer1->userHeader, 1000);
	}
	zwritea(ifltab7, pathname, &len,			
			ztransfer1->userHeader, &ztransfer1->userHeaderNumber,
			ztransfer1->values1, &ztransfer1->values1Number,						
			&plan, &recordFound);
	//  cannot check for an error without zinqir
	//if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zwritex, Loc 61")) return status;
	if (getEndian() && (zgetVersion(ifltab7) == 7)) {
		zswitchInts(ztransfer1->values1, 1000);
		zswitchInts(ztransfer1->internalHeader, 1000);
		zswitchInts(ztransfer1->header2, 1000);
		zswitchInts(ztransfer1->values3, 1000);
		zswitchInts(ztransfer1->userHeader, 1000);
	}

	ztransfer2 = zstructTransferNew(pathname, 1);
	if (!ztransfer2) {
		printf("Failed to create ztransfer2 in testIO_Interface, Loc 62!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}
	
	status =  zread(ifltab7, ztransfer2);
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zreadc, Loc 63")) return status;
	if (getEndian() && (zgetVersion(ifltab7) == 7)) {
		zswitchInts(ztransfer2->values1, ztransfer2->values1Number);
		zswitchInts(ztransfer2->userHeader, ztransfer2->userHeaderNumber);
	}

	//  remove the incompatible version 6 items
	ztransfer3 = cloneTransfereStruct(ztransfer1);
	ztransfer3->internalHeader = 0;
	ztransfer3->internalHeaderNumber = 0;
	ztransfer3->header2 = 0;
	ztransfer3->header2Number = 0;
	ztransfer3->values3 = 0;
	ztransfer3->values3Number = 0;
	ztransfer3->values2 = 0;
	ztransfer3->values2Number = 0;
	ztransfer3->numberValues = 0;
	ztransfer3->logicalNumberValues = 0;
	ztransfer3->totalExpandedSize = 0;
	ztransfer3->totalAllocatedSize = 0;
	ztransfer3->dataType = 0;
	ztransfer4 = cloneTransfereStruct(ztransfer2);
	ztransfer4->internalHeader = 0;
	ztransfer4->header2 = 0;
	ztransfer4->values2 = 0;
	status = zcompareDataSets(ifltab7, ztransfer3, ztransfer4,  1, 0, ztransfer3->pathname, "Fail in testIO_Interface, Loc 64");
	if (status) return status;
	
	zstructFree(ztransfer2);
	zstructFree(ztransfer3);
	zstructFree(ztransfer4);


	ztransfer2 = zstructTransferNew(pathname, 0);
	if (!ztransfer2) {
		printf("Failed to create ztransfer2 in testIO_Interface, Loc 65!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}

	allocateTransfereStruct(ztransfer2);
	len = (int)strlen(pathname);
	plan = 0;
	zreada (ifltab7, pathname, &len,			 			 	 
			 ztransfer2->userHeader, &ztransfer2->userHeaderMode, 
			 ztransfer2->values1, &ztransfer2->values1Mode, 
			 &plan, &recordFound);
	if (getEndian() && (zgetVersion(ifltab7) == 7)) {
		zswitchInts(ztransfer2->values1, ztransfer2->values1Number);
		zswitchInts(ztransfer2->userHeader, ztransfer2->userHeaderNumber);
	}
	status = -1;
	if (recordFound) status = 0;
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zreadc, Loc 66")) return status;


	ztransfer2->userHeaderNumber = ztransfer2->userHeaderMode;
	ztransfer2->values1Number = ztransfer2->values1Mode;
	
	//  remove the incompatible version 6 items
	ztransfer3 = cloneTransfereStruct(ztransfer1);
	ztransfer3->internalHeader = 0;
	ztransfer3->internalHeaderNumber = 0;
	ztransfer3->header2 = 0;
	ztransfer3->header2Number = 0;
	ztransfer3->values3 = 0;
	ztransfer3->values3Number = 0;
	ztransfer3->values2 = 0;
	ztransfer3->values2Number = 0;
	ztransfer3->numberValues = 0;
	ztransfer3->logicalNumberValues = 0;
	ztransfer3->totalExpandedSize = 0;
	ztransfer3->totalAllocatedSize = 0;
	ztransfer3->dataType = 0;
	ztransfer4 = cloneTransfereStruct(ztransfer2);
	ztransfer4->internalHeader = 0;
	ztransfer4->internalHeaderNumber = 0;
	ztransfer4->header2 = 0;
	ztransfer4->header2Number = 0;
	ztransfer4->values3 = 0;
	ztransfer4->values3Number = 0;
	ztransfer4->values2 = 0;
	ztransfer4->values2Number = 0;
	status = zcompareDataSets(ifltab7, ztransfer3, ztransfer4,  1, 0, ztransfer3->pathname, "Fail in testIO_Interface, Loc 67");
	if (status) return status;

	zstructFree(ztransfer1);
	zstructFree(ztransfer2);
	zstructFree(ztransfer3);
	zstructFree(ztransfer4);


		
	//  Now with version 6
	copyAndTrim (pathname, MAX_PATHNAME_LENGTH, "/Test IO Interface/B/C/D/E/zwritea6 - zreada6/", MAX_PATHNAME_LENGTH-5);

	ztransfer1 = zstructTransferNew(pathname, 0);
	if (!ztransfer1) {
		printf("Failed to create zStructTransfer in testIO_Interface, Loc 70!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}
	setTransferStruct(ztransfer1,
		fvalues, dvalues, ivalues1, ivalues2,
		ivalues3, ivalues4, ivalues5);
	
	len = (int)strlen(pathname);
	plan = 0;
	zwritea(ifltab6, pathname, &len,			
			ztransfer1->userHeader, &ztransfer1->userHeaderNumber,
			ztransfer1->values1, &ztransfer1->values1Number,						
			&plan, &recordFound);
	//  cannot check for an error without zinqir
	//if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zwritex, Loc 71")) return status;

	ztransfer2 = zstructTransferNew(pathname, 1);
	if (!ztransfer2) {
		printf("Failed to create ztransfer2 in testIO_Interface, Loc 72!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}
	
	status =  zread(ifltab6, ztransfer2);
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zreadc, Loc 73")) return status;

	//  remove the incompatible version 6 items
	ztransfer3 = cloneTransfereStruct(ztransfer1);
	ztransfer3->internalHeader = 0;
	ztransfer3->internalHeaderNumber = 0;
	ztransfer3->header2 = 0;
	ztransfer3->header2Number = 0;
	ztransfer3->values3 = 0;
	ztransfer3->values3Number = 0;
	ztransfer3->values2 = 0;
	ztransfer3->values2Number = 0;
	ztransfer3->numberValues = 0;
	ztransfer3->logicalNumberValues = 0;
	ztransfer3->totalExpandedSize = 0;
	ztransfer3->totalAllocatedSize = 0;
	ztransfer3->dataType = 0;
	ztransfer4 = cloneTransfereStruct(ztransfer2);
	ztransfer4->internalHeader = 0;
	ztransfer4->header2 = 0;
	ztransfer4->values2 = 0;
	//  On zwritea, two words are stored in internal header
	ztransfer3->internalHeaderNumber = 2;
	status = zcompareDataSets(ifltab7, ztransfer3, ztransfer4,  1, 0, ztransfer3->pathname, "Fail in testIO_Interface, Loc 74");
	if (status) return status;

	zstructFree(ztransfer2);
	zstructFree(ztransfer3);
	zstructFree(ztransfer4);;

	ztransfer2 = zstructTransferNew(pathname, 0);
	if (!ztransfer2) {
		printf("Failed to create ztransfer2 in testIO_Interface, Loc 75!\n");
		printf("Pathname: %s\n", pathname);
		return -1;
	}

	allocateTransfereStruct(ztransfer2);
	len = (int)strlen(pathname);
	plan = 1;
	zreada (ifltab6, pathname, &len,			 			 	 
			 ztransfer2->userHeader, &ztransfer2->userHeaderMode, 
			 ztransfer2->values1, &ztransfer2->values1Mode, 
			 &plan, &recordFound);
	status = -1;
	if (recordFound) status = 0;
	if (zcheckStatus(ifltab7, status, 1, "Fail in testIO_Interface, zreadc, Loc 76")) return status;


	ztransfer2->userHeaderNumber = ztransfer2->userHeaderMode;
	ztransfer2->values1Number = ztransfer2->values1Mode;
	
	//  remove the incompatible version 6 items
	ztransfer3 = cloneTransfereStruct(ztransfer1);
	ztransfer3->internalHeader = 0;
	ztransfer3->internalHeaderNumber = 0;
	ztransfer3->header2 = 0;
	ztransfer3->header2Number = 0;
	ztransfer3->values3 = 0;
	ztransfer3->values3Number = 0;
	ztransfer3->values2 = 0;
	ztransfer3->values2Number = 0;
	ztransfer3->numberValues = 0;
	ztransfer3->logicalNumberValues = 0;
	ztransfer3->totalExpandedSize = 0;
	ztransfer3->totalAllocatedSize = 0;
	ztransfer3->dataType = 0;
	ztransfer4 = cloneTransfereStruct(ztransfer2);
	ztransfer4->internalHeader = 0;
	ztransfer4->internalHeaderNumber = 0;
	ztransfer4->header2 = 0;
	ztransfer4->header2Number = 0;
	ztransfer4->values3 = 0;
	ztransfer4->values3Number = 0;
	ztransfer4->values2 = 0;
	ztransfer4->values2Number = 0;
	status = zcompareDataSets(ifltab7, ztransfer3, ztransfer4,  1, 0, ztransfer3->pathname, "Fail in testIO_Interface, Loc 77");
	if (status) return status;

	zstructFree(ztransfer1);
	zstructFree(ztransfer2);
	zstructFree(ztransfer3);
	zstructFree(ztransfer4);

	// Good to this point


	printf("Passed testIO_Interface\n");
	return 0;
}

void setTransferStruct(zStructTransfer *ztransfer,
						float *fvalues,
						double *dvalues,
						int *ivalues1,
						int *ivalues2,
						int *ivalues3,
						int *ivalues4,
						int *ivalues5)
{

	ztransfer->internalHeader = ivalues1;
	ztransfer->internalHeaderNumber = 800;
	ztransfer->header2 = ivalues2;
	ztransfer->header2Number = 399;
	ztransfer->values3 = ivalues3;
	ztransfer->values3Number = 5;
	ztransfer->userHeader = ivalues4;
	ztransfer->userHeaderNumber = 721;

	ztransfer->values1 = (void *)fvalues;
	ztransfer->values1Number = 1000;
	ztransfer->values2 = (void *)dvalues;
	ztransfer->values2Number = 2000;

	ztransfer->numberValues = 1234;
	ztransfer->logicalNumberValues = 5678;
	ztransfer->totalAllocatedSize = 3456;
	ztransfer->totalExpandedSize = 6543;
	ztransfer->dataType = 999;
}

void allocateTransfereStruct(zStructTransfer *ztransfer)
{
	ztransfer->internalHeaderMode = 1000;
	ztransfer->header2Mode = 1000;
	ztransfer->values3Mode = 1000;
	ztransfer->userHeaderMode = 1000;
	ztransfer->values1Mode = 5000;
	ztransfer->values2Mode = 5000;

	ztransfer->internalHeaderNumber = 1000;
	ztransfer->header2Number = 1000;
	ztransfer->values3Number = 1000;
	ztransfer->userHeaderNumber = 1000;
	ztransfer->values1Number = 5000;
	ztransfer->values2Number = 5000;

	ztransfer->internalHeader = (int *)calloc(ztransfer->internalHeaderNumber, 4);
	ztransfer->allocated[zSTRUCT_TRANS_internalHeader] = 1;
	ztransfer->header2 = (int *)calloc(ztransfer->header2Number, 4);
	ztransfer->allocated[zSTRUCT_TRANS_header2] = 1;
	ztransfer->values3 = (int *)calloc(ztransfer->values3Number, 4);
	ztransfer->allocated[zSTRUCT_TRANS_values3] = 1;
	ztransfer->userHeader = (int *)calloc(ztransfer->userHeaderNumber, 4);
	ztransfer->allocated[zSTRUCT_userHeader] = 1;
	ztransfer->values1 = (int *)calloc(ztransfer->values1Number, 4);
	ztransfer->allocated[zSTRUCT_TRANS_values1] = 1;
	ztransfer->values2 = (int *)calloc(ztransfer->values2Number, 4);
	ztransfer->allocated[zSTRUCT_TRANS_values2] = 1;

}


zStructTransfer *cloneTransfereStruct(zStructTransfer *ztransfer1)
{
	//  Warning - shallow copy only!
	zStructTransfer *ztransfer2;
	
	ztransfer2 = zstructTransferNew(ztransfer1->pathname, 0);

	ztransfer2->pathnameLength = ztransfer1->pathnameLength;

	ztransfer2->internalHeader = ztransfer1->internalHeader;
	ztransfer2->internalHeaderMode = ztransfer1->internalHeaderMode;
	ztransfer2->internalHeaderNumber = ztransfer1->internalHeaderNumber;
	ztransfer2->header2 = ztransfer1->header2;
	ztransfer2->header2Mode = ztransfer1->header2Mode;
	ztransfer2->header2Number = ztransfer1->header2Number;
	ztransfer2->values3 = ztransfer1->values3;
	ztransfer2->values3Mode = ztransfer1->values3Mode;
	ztransfer2->values3Number = ztransfer1->values3Number;
	ztransfer2->userHeader = ztransfer1->userHeader;
	ztransfer2->userHeaderMode = ztransfer1->userHeaderMode;
	ztransfer2->userHeaderNumber = ztransfer1->userHeaderNumber;

	ztransfer2->values1 = ztransfer1->values1;
	ztransfer2->values1Mode = ztransfer1->values1Mode;
	ztransfer2->values1Number = ztransfer1->values1Number;
	ztransfer2->values2 = ztransfer1->values2;
	ztransfer2->values2Mode = ztransfer1->values2Mode;
	ztransfer2->values2Number = ztransfer1->values2Number;

	ztransfer2->numberValues = ztransfer1->numberValues;
	ztransfer2->logicalNumberValues = ztransfer1->logicalNumberValues;
	ztransfer2->totalAllocatedSize = ztransfer1->totalAllocatedSize;
	ztransfer2->totalExpandedSize = ztransfer1->totalExpandedSize;
	ztransfer2->dataType = ztransfer1->dataType;

	return ztransfer2;
}

