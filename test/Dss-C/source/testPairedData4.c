#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#include "heclib.h"
#include "hecdss7.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "TestDssC.h"

//  Test Paired Data in DSS-7
//

int testPairedData4(long long *ifltab)
{
	

	//  1000000 rows by 1 columns (curves)
	float *floatOridnates;
	float *floatValues;
	clock_t begin, end;
	
	zStructPairedData *pdsf1=0;
	zStructPairedData *pdsf2=0;


	char mess[100];
	int i, j;
	int status;
	int nrows, ncols;
	char pathname1[200];

	int zero = 0;
	int one = 1;
	int two = 2;
	int userHeaderNumber;
	int loop;
	// LARGE_INTEGER StartingTime, EndingTime, ElapsedMicroseconds;
	// LARGE_INTEGER Frequency;

	// DWORD value = MAX_PATH;
	// TCHAR  buff[MAX_PATH];
	// DWORD dw;
	char cbuff[40];

	nrows = 1000000;
	ncols = 1;




	stringCopy(pathname1, sizeof(pathname1), "/a/b/c1-c2/doubles/PD/Test 1/", sizeof(pathname1));
	
	/*
	status = zcheck(ifltab, pathname1);
	if (zisError(status)) {
		//  An error code 
		return status;
	}
	if (status == STATUS_RECORD_FOUND) {
		zdelete(ifltab, pathname1);
	}
	*/
	
	/*
	floatOridnates = (float*)calloc(nrows, 4);
	floatValues = (float*)calloc(nrows, 4);

	for (i=0;i<nrows; i++) {
		floatOridnates[i] = (float)(i + 1);
	}

	for (i=0; i<ncols; i++) {	
		for (j=0; j<nrows; j++) {
			floatValues[j] = (float)((i+1) * 1000) + (float)j;
		}
	}
	userHeaderNumber = 0;


		
	pdsf1 = zstructPdNewFloats(pathname1, floatOridnates, (void *)floatValues, 
							 nrows, ncols, 
							 "Feet", "INST-VAL", 
							 "CFS", "INST-VAL");

	status = zpdStore(ifltab, pdsf1, 0);
	stringCopy(mess, sizeof(mess),  "testPairedData4 Loc 3, zpdStore status ", _TRUNCATE); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	pdsf2 = zstructPdNew(pathname1);
	status = zpdRetrieve(ifltab, pdsf2, 1);
	stringCopy(mess, sizeof(mess),  "testPairedData4 Loc 4, zpdRetrieve status ", _TRUNCATE); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = comparePDs (ifltab, pdsf1, pdsf2, "Paired data 4, Loc 4, floats");
	if (status != STATUS_OKAY) return status;


	if (pdsf1) zstructFree(pdsf1);
	pdsf1 = 0;
	if (pdsf2) zstructFree(pdsf2);
	pdsf2 = 0;

	free(floatOridnates);
	free(floatValues);

	*/
	
	nrows = 500000;
	floatOridnates = (float*)calloc(nrows, 4);
	floatValues = (float*)calloc(1, 4);

	for (i=0;i<nrows; i++) {
		floatOridnates[i] = (float)(i + 1);
	}

	for (i=0; i<ncols; i++) {	
		for (j=0; j<nrows; j++) {
	//		floatValues[j] = (float)((i+1) * 1000) + (float)j;
		}
	}
	userHeaderNumber = 0;

	// QueryPerformanceFrequency(&Frequency); 
	// QueryPerformanceCounter(&StartingTime);
	begin = clock();
	pdsf1 = zstructPdNewFloats(pathname1, floatOridnates, (float *)floatValues, 
							 nrows, ncols, 
							 "Feet", "INST-VAL", 
							 "CFS", "INST-VAL");
	//zset("mlvl", "", 15);
	status = zpdStore(ifltab, pdsf1, 10);
	//return 0;
	stringCopy(mess, sizeof(mess),  "testPairedData4 Loc 3, zpdStore status ", _TRUNCATE); 
	//zset("mlvl", "", 12);
    end = clock();
	// QueryPerformanceCounter(&EndingTime);
	// ElapsedMicroseconds.QuadPart = EndingTime.QuadPart - StartingTime.QuadPart;
	// ElapsedMicroseconds.QuadPart *= 1000000;
	// ElapsedMicroseconds.QuadPart /= Frequency.QuadPart;
	// longWithCommas(cbuff, sizeof(cbuff), (long long)ElapsedMicroseconds.QuadPart);
	// //printf("%d, time = %s   %s\n\n", i, cbuff, tss1->pathname);
	
	printf("Write original single record (%d rows) time = %f s\n", nrows, (double)(end - begin) / CLOCKS_PER_SEC);


	// QueryPerformanceCounter(&StartingTime);
	for (i=0; i<nrows; i++) {	

		floatOridnates[0] = (float)i;
		floatValues[0] = (float)i;

		pdsf1->startingOrdinate = i+1;
		pdsf1->endingOrdinate = i+1;

		status = zpdStore(ifltab, pdsf1, 0);		
		checknumbers_(&zero, &status, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) {
			return status;
		}

		j = i / 10000;
		j *= 10000;
		if (j == i) {
			//printf("At row %d\n", i);
			if (i > 0) {
				// QueryPerformanceCounter(&EndingTime);
				// ElapsedMicroseconds.QuadPart = EndingTime.QuadPart - StartingTime.QuadPart;
				// ElapsedMicroseconds.QuadPart *= 1000000;
				// ElapsedMicroseconds.QuadPart /= Frequency.QuadPart;
				// longWithCommas(cbuff, sizeof(cbuff), (long long)ElapsedMicroseconds.QuadPart);
				printf("append at row %d\\n", i);
				// QueryPerformanceCounter(&StartingTime);
			}
		}

	}	

	return 0; 
}
