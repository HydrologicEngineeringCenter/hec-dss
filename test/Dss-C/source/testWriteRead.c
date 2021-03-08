#include <stdio.h>
#include <string.h>

#include "heclib.h"
#include "hecdssInternal.h"
#include "zStructTimeSeries.h"
#include "TestDssC.h"



	//  Test write and read using a maximum length pathname

int testWriteRead(long long *ifltab)
{
	char path1[MAX_PATHNAME_LENGTH];	

	char cpart[65];
	char slash[2];
	int status;
	int i;
	int count;
	int dummy[1];
	int ndummy;
	int number;
	int *buffer=0; long long bufferControl[4] ={0,0,0,0};
	int values[1000];
	int valuesRead[1000];

	for (i=0; i<1000; i++) {
		values[i] = i;
	}

	//  Test a full pathname with Max Path length
	count = 97;
	for (i=0; i<64; i++) {
		cpart[i] = (char)count++;
		if (count >= 123) count=97;
	}
	cpart[64] =  '\0';

	dummy[0] = 0;
	slash[0] = '/';
	slash[1] = '\0';
	path1[0] = '\0';
	stringCat (path1, MAX_PATHNAME_LENGTH, slash, 1);
	stringCat (path1, MAX_PATHNAME_LENGTH, cpart, 64);
	stringCat (path1, MAX_PATHNAME_LENGTH, slash, 1);
	stringCat (path1, MAX_PATHNAME_LENGTH, cpart, 64);
	stringCat (path1, MAX_PATHNAME_LENGTH, slash, 1);
	stringCat (path1, MAX_PATHNAME_LENGTH, cpart, 64);
	stringCat (path1, MAX_PATHNAME_LENGTH, slash, 1);
	stringCat (path1, MAX_PATHNAME_LENGTH, cpart, 64);
	stringCat (path1, MAX_PATHNAME_LENGTH, slash, 1);
	stringCat (path1, MAX_PATHNAME_LENGTH, cpart, 64);
	stringCat (path1, MAX_PATHNAME_LENGTH, slash, 1);
	stringCat (path1, MAX_PATHNAME_LENGTH, cpart, 64);
	stringCat (path1, MAX_PATHNAME_LENGTH, slash, 1);

	status = zwritec (ifltab, path1,		
		dummy, 0,
		dummy, 0,
		dummy, 0,
		dummy, 0,
		values, 1000, 
		0,0,
		1000, 1000, 
		0, 0, 5);

	if (status != STATUS_OKAY) {
		printf("Write on testWriteRead Failed!\n");
		return status;
	}



	status = zreadc(ifltab, path1,
		dummy, 0, &ndummy,
		dummy, 0, &ndummy,
		dummy, 0, &ndummy,
		dummy, 0, &ndummy,
		valuesRead, 1000, &number,
		dummy, 0, &ndummy,
		&ndummy, &ndummy,
		&ndummy, &ndummy, &ndummy);



	if (status != STATUS_OKAY) {
		printf("Read on testWriteRead Failed!\n");
		return status;
	}

	if (number != 1000) {
		printf("Number Read does not match, testWriteRead Failed!\n");
		return -1;
	}

	for (i=0; i<1000; i++) {
		if (values[i] != valuesRead[i]) {
			printf("Values Read does not match those written, testWriteRead Failed!\n");
			return -1;
		}
	}

	return 0;
}
