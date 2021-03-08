//#include <io.h>
#include <stdio.h>
#include <string.h>

#include "heclib.h"
#include "TestDssC.h"



int testConversion()
{
	int DEBUG=0;
	int status;
	int number;
	char c[5];
	char filename_orig[200];
	char filename6[200];
	char filename6a[200];
	char filename7[200];
	char filename7a[200];

	long long ifltab6[600];
	long long ifltab7[600];

	memset(ifltab6,0,600*sizeof( long long));
	memset(ifltab7,0,600*sizeof( long long));
	


	stringCopy(filename_orig, sizeof(filename_orig), "sample.dss", _TRUNCATE);
	stringCopy(filename6, sizeof(filename6), "sample6.dss", _TRUNCATE);
	stringCopy(filename6a, sizeof(filename6a), "sample6a.dss", _TRUNCATE);
	stringCopy(filename7, sizeof(filename7), "sample7.dss", _TRUNCATE);
	stringCopy(filename7a, sizeof(filename7a), "sample7a.dss", _TRUNCATE);

	status = zopen6(ifltab6, filename_orig);
	if (status) {
		printf("Error opening %s, status = %d!\n", filename_orig, status);
		return  status;
	}

	zinquireChar(ifltab6, "nrec", c, (int)sizeof(c), &number);
	if (number <= 0) {
		printf("\n\nFirst file for conversion test is not correct.\n");
		printf("It should be %s\na version 6 file, copied from HEC-DSSVue/samples.\n\n", filename_orig);
		return -2;
	}

	remove(filename7);
	status = zopen7(ifltab7, filename7);	
	if (status) {
		printf("Error opening %s, status = %d!\n", filename7, status);
		return  status;
	}
	//zset("mlvl", " ", 5);

	status  = zcopyFile(ifltab6, ifltab7, 0);
    if (status) {
		printf("Error during conversion from 6 to 7, %s, status = %d!\n", filename7, status);
		return  status;
	}
    
	zclose(ifltab6);

	remove(filename6a);
	status = zopen6(ifltab6, filename6a);	
	if (status) {
		printf("Error opening %s, status = %d!\n", filename6a, status);
		return  status;
	}

	status  = zcopyFile(ifltab7, ifltab6, 0);
    if (status) {
		printf("Error during conversion from 7 to 6, %s, status = %d!\n", filename6a, status);
		return  status;
	}
    
	zclose(ifltab6);
	zclose(ifltab7);

	remove(filename7a);

	status = zconvertVersion(filename6a, filename7a);
	if (status) {
		printf("Error during conversion from 6 to 7, Loc 2, status = %d!\n", status);
		return  status;
	}

	status = zconvertVersion("sample7a.dss", "sample6a.dss");
	if (status) {
		printf("Error during conversion from 6 to 7, Loc 3, status = %d!\n", status);
		return  status;
	}
		
	zset("mlvl", " ", 4);

	printf("File conversion from 6 to 7 and 7 to 6 passed.\n");

	return 0;
}