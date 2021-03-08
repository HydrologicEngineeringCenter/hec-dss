#include <stdio.h>
#include <string.h>

#include "heclib.h"
#include "hecdssInternal.h"
#include "TestDssC.h"



	//  Test array write and read 

int testArrayWriteRead(long long *ifltab)
{
	zStructArray *struct1;
	zStructArray *struct2;
	zStructArray *struct3;
	zStructArray *struct4;
	zStructArray *struct5;
	zStructArray *struct6;

	char path1[MAX_PATHNAME_LENGTH];
	char path2[MAX_PATHNAME_LENGTH];
	char path3[MAX_PATHNAME_LENGTH];
	int status;
	int i;

	int intvalues[1000];
	float floatvalues[1000];
	double doublevalues[1000];

	int nvalues = 1000;

	for (i=0; i<nvalues; i++) {
		intvalues[i] = i;
		floatvalues[i] = (float)i;
		doublevalues[i] = (double)i;
	}

	stringCopy(path1, sizeof(path1), "/a/b/c/d/generic array/ints/", 50); 
	stringCopy(path2, sizeof(path2), "/a/b/c/d/generic array/floats/", 50);
	stringCopy(path3, sizeof(path3), "/a/b/c/d/generic array/doubles/", 50);

	struct1 = zstructArrayNew(path1);
	struct2 = zstructArrayNew(path2);
	struct3 = zstructArrayNew(path3);

	struct4 = zstructArrayNew(path1);
	struct5 = zstructArrayNew(path2);
	struct6 = zstructArrayNew(path3);

	
//  int zarrayStore(long long *ifltab, zStructArray *arrayStruct)
	struct1->intArray = intvalues;
	struct1->numberIntArray = nvalues;
	status = zarrayStore (ifltab, struct1);
	if (status != STATUS_OKAY) {
		printf("Write on testArrayWriteRead ints Failed!\n");
		return status;
	}

	///////////////////////////////
	//zset("MLVL", "", 15);
	//zdelete(ifltab, path1);
	//status = zarrayStore (ifltab, struct1);
	///////////////////////////////

	struct2->floatArray = floatvalues;
	struct2->numberFloatArray = nvalues;
	status = zarrayStore (ifltab, struct2);
	if (status != STATUS_OKAY) {
		printf("Write on testArrayWriteRead	floats Failed!\n");
		return status;
	}

	struct3->doubleArray = doublevalues;
	struct3->numberDoubleArray = nvalues;
	status = zarrayStore (ifltab, struct3);
	if (status != STATUS_OKAY) {
		printf("Write on testArrayWriteRead doubles Failed!\n");
		return status;
	}


//int zarrayRetrieve(long long *ifltab, zStructArray *arrayStruct)
	status = zarrayRetrieve(ifltab, struct4);
	if (status != STATUS_RECORD_FOUND) {
		printf("Read on testArrayWriteRead ints Failed!\n");
		return status;
	}
	status = zcompareDataSets(ifltab, struct1, struct4, 1, 0, struct1->pathname, "Fail in testArrayWriteRead, Location 10");
	if (status) return status;

	status = zarrayRetrieve(ifltab, struct5);
	if (status != STATUS_RECORD_FOUND) {
		printf("Read on testArrayWriteRead floats Failed!\n");
		return status;
	}
	status = zcompareDataSets(ifltab, struct2, struct5, 1, 0, struct2->pathname, "Fail in testArrayWriteRead, Location 11");
	if (status) return status;

	status = zarrayRetrieve(ifltab, struct6);
	if (status != STATUS_RECORD_FOUND) {
		printf("Read on testArrayWriteRead doubles Failed!\n");
		return status;
	}
	status = zcompareDataSets(ifltab, struct3, struct6, 1, 0, struct2->pathname, "Fail in testArrayWriteRead, Location 12");
	if (status) return status;	

	zstructFree(struct1);
	zstructFree(struct2);
	zstructFree(struct3);
	zstructFree(struct4);
	zstructFree(struct5);
	zstructFree(struct6);
	
	printf("Completed array store retrieve test successfully!\n");

	return 0;
}
