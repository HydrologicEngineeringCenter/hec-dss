#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "hecdss7.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "TestDssC.h"

//  Test Paired Data in DSS-7
//


int testPairedData(long long *ifltab)
{
	

	int len;
	double ddummy[1];
	//  100 rows by 50 columns (curves)
	float floatOridnates[100];
	float floatValues[50][100];
	double doubleOridnates[100];
	double doubleValues[50][100];
	char label[100];
	char labelArray[5000];
	int labelLength;
	int userHeader[100];

	zStructPairedData *pdsd1=0;
	zStructPairedData *pdsd2=0;
	zStructPairedData *pdsd3=0;
	zStructPairedData *pdsd4=0;
	zStructPairedData *pdsf1=0;
	zStructPairedData *pdsf2=0;

	char mess[100];
	int i, j;
	int status;
	int nrows, ncols;
	char pathname1[200];
	char pathname2[200];
	char pathname3[200];
	char pathname4[200];
	char pathname5[200];
	char pathname6[200];
	char pathname7[200];
	int zero = 0;
	int one = 1;
	int two = 2;
	int userHeaderNumber;

	nrows = 100;
	ncols = 50;


	stringCopy(pathname1, sizeof(pathname1), "/a/b/c1-c2/doubles/PD/Test 1/", sizeof(pathname1));
		
	status = zcheck(ifltab, pathname1);
	if (zisError(status)) {
		//  An error code 
		return status;
	}
	if (status == STATUS_RECORD_FOUND) {
		zdelete(ifltab, pathname1);
	}

	for (i=0;i<nrows; i++) {
		doubleOridnates[i] = (double)(i + 1);
		floatOridnates[i] = (float)(i + 1);
		userHeader[i] = i;
	}

	for (i=0; i<ncols; i++) {	
		for (j=0; j<nrows; j++) {
			doubleValues[i][j] = (double)((i+1) * 1000) + (double)j;
			floatValues[i][j] = (float)((i+1) * 1000) + (float)j;
		}
	}
	userHeaderNumber = nrows;

	labelLength = 0;
	for (j=0; j<ncols; j++) {
		_snprintf_s(label, sizeof(label), _TRUNCATE, "Column %d", j);
		len = (int)strlen(label);
		stringCopy(&labelArray[labelLength], sizeof(labelArray)-labelLength, label, len);
		labelLength += len + 1;
	}
	
	stringCopy(pathname2, sizeof(pathname2), "/a/b/c1-c2/doubles/PD/Test 1/", sizeof(pathname2));
	pdsd1 = zstructPdNewDoubles(pathname2, doubleOridnates, (void *)doubleValues, 
							 nrows, ncols, 
							 "Feet", "INST-VAL", 
							 "CFS", "INST-VAL");
	pdsd1->boolIndependentIsXaxis = 1;
	status = zpdStore(ifltab, pdsd1, 0);
	stringCopy(mess, sizeof(mess),  "testPairedData Loc 1, zpdStore status ", _TRUNCATE); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;
	


	pdsd2 = zstructPdNew(pathname2);
	status = zpdRetrieve(ifltab, pdsd2, 2);
	stringCopy(mess, sizeof(mess),  "testPairedData Loc 2, zpdRetrieve status ", _TRUNCATE); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = comparePDs (ifltab, pdsd1, pdsd2, "Paired data basic, Loc 2, doubles");

	if (status) return status;


	//   Same with floats
	stringCopy(pathname3, sizeof(pathname3), "/a/b/c1-c2/floats/PD/Test 1/", sizeof(pathname3));
		
	pdsf1 = zstructPdNewFloats(pathname3, floatOridnates, (void *)floatValues, 
							 nrows, ncols, 
							 "Feet", "INST-VAL", 
							 "CFS", "INST-VAL");

	status = zpdStore(ifltab, pdsf1, 0);
	stringCopy(mess, sizeof(mess),  "testPairedData Loc 3, zpdStore status ", _TRUNCATE); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	pdsf2 = zstructPdNew(pathname3);
	status = zpdRetrieve(ifltab, pdsf2, 1);
	stringCopy(mess, sizeof(mess),  "testPairedData Loc 4, zpdRetrieve status ", _TRUNCATE); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = comparePDs (ifltab, pdsf1, pdsf2, "Paired data basic, Loc 4, floats");
	if (status != STATUS_OKAY) return status;

	if (pdsd2) zstructFree(pdsd2);
	pdsd2 = 0;

	//  now doubles from floats

	stringCopy(pathname4, sizeof(pathname4), "/a/b/c1-c2/floats/PD/Test 1/", sizeof(pathname4));
	pdsd2 = zstructPdNew(pathname4);
	status = zpdRetrieve(ifltab, pdsd2, 2);
	stringCopy(mess, sizeof(mess),  "testPairedData Loc 5, zpdRetrieve status ", _TRUNCATE); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;
	status = comparePDs (ifltab, pdsd1, pdsd2, "Paired data read doubles from floats, Loc 5");
	if (status != STATUS_OKAY) return status;

	if (pdsf2) zstructFree(pdsf2);
	pdsf2 = 0;

	//  now floats from doubles
	stringCopy(pathname5, sizeof(pathname5), "/a/b/c1-c2/doubles/PD/Test 1/", sizeof(pathname5));

	pdsf2 = zstructPdNew(pathname5);
	status = zpdRetrieve(ifltab, pdsf2, 1);
	stringCopy(mess, sizeof(mess),  "testPairedData Loc 6, zpdRetrieve status ", _TRUNCATE); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;
	status = comparePDs (ifltab, pdsf1, pdsf2, "Paired data read floats from doubles, Loc 6");
	if (status != STATUS_OKAY) return status;



	if (pdsd1) zstructFree(pdsd1);
	pdsd1 = 0;
	if (pdsd2) zstructFree(pdsd2);
	pdsd2 = 0;
	if (pdsf1) zstructFree(pdsf1);
	pdsf1 = 0;
	if (pdsf2) zstructFree(pdsf2);
	pdsf2 = 0;


	if (status) return status;

	//  Now add labels
	stringCopy(pathname6, sizeof(pathname6), "/a/b/c1-c2/doubles/PD/labels/", sizeof(pathname6));
	pdsd1 = zstructPdNewDoubles(pathname6, doubleOridnates, (void *)doubleValues, 
							 nrows, ncols, 
							 "Feet", "INST-VAL", 
							 "CFS", "INST-VAL");
	pdsd1->labels = labelArray;
	pdsd1->labelsLength = labelLength;

	pdsd1->xprecision = 2;
	pdsd1->yprecision = 2;
	pdsd1->boolIndependentIsXaxis = 1;
	//  ADD TIME ZONE!!!!!!!!

	status = zpdStore(ifltab, pdsd1, 0);
	stringCopy(mess, sizeof(mess),  "testPairedData Loc 100, zpdStore status ", _TRUNCATE); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	pdsd2 = zstructPdNew(pathname6);
	status = zpdRetrieve(ifltab, pdsd2, 2);
	stringCopy(mess, sizeof(mess),  "testPairedData Loc 101, zpdRetrieve status ", _TRUNCATE); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = comparePDs (ifltab, pdsd1, pdsd2, "Paired data basic, Loc 101, doubles");

	if (status) return status;

	pdsd1->userHeader = userHeader;
	pdsd1->userHeaderNumber = userHeaderNumber;

	status = zpdStore(ifltab, pdsd1, 0);
	stringCopy(mess, sizeof(mess),  "testPairedData Loc 110, zpdStore status ", _TRUNCATE); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	if (pdsd2) zstructFree(pdsd2);
	pdsd2 = 0;
	pdsd2 = zstructPdNew(pathname6);
	status = zpdRetrieve(ifltab, pdsd2, 2);
	stringCopy(mess, sizeof(mess),  "testPairedData Loc 111, zpdRetrieve status ", _TRUNCATE); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = comparePDs (ifltab, pdsd1, pdsd2, "Paired data basic, Loc 111, doubles");

	if (status) return status;

	//  Now test where we allocate space at the beginning and write a curve each call

	ddummy[0] = 0.0;
	stringCopy(pathname7, sizeof(pathname7), "/a/b/c1-c2/doubles/PD/write curves separately/", sizeof(pathname7));
	pdsd3 = zstructPdNewDoubles(pathname7, doubleOridnates, (void *)doubleValues, 
							 nrows, ncols, 
							 "Feet", "INST-VAL", 
							 "CFS", "INST-VAL");

	pdsd3->labels = labelArray;
	pdsd3->labelsLength = labelLength;
	pdsd3->xprecision = 2;
	pdsd3->yprecision = 2;
	pdsd3->boolIndependentIsXaxis = 1;
	pdsd3->userHeader = userHeader;
	pdsd3->userHeaderNumber = userHeaderNumber;
	//zset("mlvl", "", 15);

	status = zpdStore(ifltab, pdsd3, 10);
	stringCopy(mess, sizeof(mess),  "testPairedData Loc 201, zpdStore status ", _TRUNCATE); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	for (i=0; i<ncols; i++) {
		zstructFree(pdsd3);
		pdsd3 = zstructPdNew(pathname7);
		pdsd3->startingCurve = i+1;
		pdsd3->endingCurve = pdsd3->startingCurve;
		pdsd3->numberOrdinates = nrows;
		pdsd3->doubleValues = &doubleValues[i][0];
		_snprintf_s(label, sizeof(label), _TRUNCATE, "Column %d", i);
		len = (int)strlen(label);
		pdsd3->labels = label;
		pdsd3->labelsLength = len;
		status = zpdStore(ifltab, pdsd3, 0);
		stringCopy(mess, sizeof(mess),  "testPairedData Loc 202, zpdStore status ", _TRUNCATE); 
		checknumbers_(&zero, &status, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}

	if (pdsd2) zstructFree(pdsd2);
	pdsd2 = 0;
	pdsd2 = zstructPdNew(pathname7);
	status = zpdRetrieve(ifltab, pdsd2, 2);
	stringCopy(mess, sizeof(mess),  "testPairedData Loc 210, zpdRetrieve status ", _TRUNCATE); 
	checknumbers_(&zero, &status, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	status = comparePDs (ifltab, pdsd1, pdsd2, "Paired data multiple writes, Loc 210, doubles");


	if (status) return status;

	

	if (pdsd1) zstructFree(pdsd1);
	pdsd1 = 0;
	if (pdsd2) zstructFree(pdsd2);
	pdsd2 = 0;
	if (pdsd3) zstructFree(pdsd3);
	pdsd3 = 0;
	if (pdsd4) zstructFree(pdsd4);
	pdsd4 = 0;
	if (pdsf1) zstructFree(pdsf1);
	pdsf1 = 0;
	if (pdsf2) zstructFree(pdsf2);
	pdsf2 = 0;
	
	printf("Completed testPairedData test successfully!\n");

	return 0; 
}
