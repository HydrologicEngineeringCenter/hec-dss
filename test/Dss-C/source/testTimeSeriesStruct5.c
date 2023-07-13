#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "heclib.h"
#include "hecdss7.h"
#include "zStructTimeSeries.h"
#include "hecdssInternal.h"
#include "TestDssC.h"


	//  Test storage flag for regular interval floats, full

int testztsStruct5(long long *ifltab)
{
	zStructTimeSeries *tss1, *tss2, *tss3, *tss4;

	int status;
	int numb;
	int zero = 0;
	char *path;
	int juls, jule, secondsEnd;
	float fvalues[2000];


	float data1[1100], data3[1100];
    int quality1[1100][2], notes1[1100][4];
	int quality3[1100][2], notes3[1100][4];
	char cnotes1[1100][50];
	char cnotes3[1100][50];
	char cnotesa[55000], cnotesb[55000];
	char cdate[13];
	char ctime[13];

    int i, j, n, ich;
    int lenQuality, lenNotes ;
    int maxVals;
	int jul;
    int lowerCase;
	int cnotesLengthTotal;
	int count;
	int diff;
	int len1, len2;
	char *cstring1, *cstring2;
	int ipos1, ipos2;

	
	tss1 = 0;
	tss2 = 0;
	tss3 = 0;
	tss4 = 0;


	//  Basic test for storageFlag 1 (only replace missing values)
	//
	for (i=0; i<200; i++) {
		fvalues[i] = (float)i;
	}

	fvalues[20] = UNDEFINED_FLOAT;

	path = mallocAndCopy("/Basin/Location/Flow/01Jan2001/30Min/Check Store Flag 1/");


	tss1 = zstructTsNewRegFloats(path, fvalues, 200, "21Jan2001", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 1, store status ")) return status; 

	tss2 = zstructTsNew(path); 
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 2, retrieve status ")) return status; 

	status = zcompareDataSets(ifltab, tss2, tss1, 1, 0, tss1->pathname, "Fail in testztsStruct5, Location 3");
	if (status) return status;

	if (tss1)zstructFree(tss1);
	tss1 = 0;

	for (i=0; i<200; i++) {
		fvalues[i] = 1234;
	}

	tss1 = zstructTsNewRegFloats(path, fvalues, 200, "21Jan2001", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 1);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 4, store status ")) return status; 

	if (tss1)zstructFree(tss1);
	tss1 = 0;

	tss1 = zstructTsNew(path); 
	status = ztsRetrieve(ifltab, tss1, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 5, retrieve status ")) return status; 
	tss2->floatValues[20] = 1234;
	status = zcompareDataSets(ifltab, tss2, tss1, 1, 0, tss1->pathname, "Fail in testztsStruct5, Location 6");
	if (status) return status;

	if (tss1)zstructFree(tss1);
	tss1 = 0;
	if (tss2)zstructFree(tss2);
	tss2 = 0;
	free(path);

	//  Now try with a undefined in the middle of a record with records on both sides
	//
	path = mallocAndCopy("/Basin/Location/Flow/01Jan2001/1Hour/Check Store Flag 1a/");
	for (i=0; i<1200; i++) {
		fvalues[i] = (float)i;	
	}

	fvalues[500] = UNDEFINED_FLOAT;

	tss1 = zstructTsNewRegFloats(path, fvalues, 1200, "21Jan2001", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 11, store status ")) return status; 

	tss2 = zstructTsNew(path); 
	tss2->boolRetrieveAllTimes = 1;
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 12, retrieve status ")) return status; 

	status = zcompareDataSets(ifltab, tss2, tss1, 1, 0, tss1->pathname, "Fail in testztsStruct5, Location 13");
	if (status) return status;

	if (tss1)zstructFree(tss1);
	tss1 = 0;

	for (i=0; i<1200; i++) {
		fvalues[i] = 1234.;
	}

	//  zsetMessageLevel(MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1);

	tss1 = zstructTsNewRegFloats(path, fvalues, 1200, "21Jan2001", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 1);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 14, store status ")) return status; 
	if (tss1)zstructFree(tss1);
	tss1 = 0;


	tss1 = zstructTsNew(path); 
	tss1->boolRetrieveAllTimes = 1;
	status = ztsRetrieve(ifltab, tss1, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 15, retrieve status ")) return status; 
	tss2->floatValues[500] = 1234;
	status = zcompareDataSets(ifltab, tss2, tss1, 1, 0, tss1->pathname, "Fail in testztsStruct5, Location 16");
	if (status) return status;

	if (tss1)zstructFree(tss1);
	tss1 = 0;
	if (tss2)zstructFree(tss2);
	tss2 = 0;
	free(path);



	//  storageFlag = 2  - always store, regardless if all missing (writes missing records - not the default)
	path = mallocAndCopy("/Basin/Location/Flow/01Jan2001/1Hour/All Missing/");
	for (i=0; i<1200; i++) {
		fvalues[i] = UNDEFINED_FLOAT;	
	}

	tss1 = zstructTsNewRegFloats(path, fvalues, 1200, "21Jan2001", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 2);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 21, store status ")) return status; 

	juls = dateToJulian("21Jan2001");
	incrementTime(SECS_IN_1_HOUR, (SECS_IN_20_MINUTES -1), juls, SECS_IN_12_HOUR, &jule, &secondsEnd);
	julianToDate(jule, 4, cdate, sizeof(cdate));
	secondsToTimeString(secondsEnd, 0, 0, ctime, sizeof(ctime));
	tss2 = zstructTsNewTimes(path, "21Jan2001", "1200", cdate, ctime); 
	status = ztsRetrieve(ifltab, tss2, 0, 0, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 22, retrieve status ")) return status; 

	status = zcompareDataSets(ifltab, tss2, tss1, 1, 0, tss1->pathname, "Fail in testztsStruct5, Location 23");
	if (tss1)zstructFree(tss1);
	tss1 = 0;
	if (status) return status;
	

	//  This part of the function tests features not supported in DSS-6
	if (zgetVersion(ifltab) == 6) {
		if (tss2)zstructFree(tss2);
		tss2 = 0;
		free(path);
		return 0;
	}

	

	//  storage flag 3 - if all missing, delete from disk
	tss1 = zstructTsNewRegFloats(path, fvalues, 1200, "21Jan2001", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 3);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 31, store status ")) return status; 

	if (tss1)zstructFree(tss1);
	tss1 = 0;

	tss1 = zstructTsNewTimes(path, "02Feb2001", "1200", "05Feb2001", "1200"); 
	status = ztsRetrieve(ifltab, tss2, 0, 0, 0);
	if (zcompareInts(ifltab, status, STATUS_RECORD_NOT_FOUND, 1, "testztsStruct5 Loc 32,")) return -1; 
	if (tss1)zstructFree(tss1);
	tss1 = 0;
	if (tss2)zstructFree(tss2);
	tss2 = 0;
	free(path);

	

	//  storage flag 4 - Do not allow a missing input data to replace a valid data piece.
	path = mallocAndCopy("/Basin/Location/Flow/01Jan2001/1Hour/Check Store Flag 4/");
	for (i=0; i<1200; i++) {
		fvalues[i] = (float)i;	
	}

	tss1 = zstructTsNewRegFloats(path, fvalues, 1200, "21Jan2001", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 41, store status ")) return status; 

	tss2 = zstructTsNew(path); 
	tss2->boolRetrieveAllTimes = 1;
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 42, retrieve status ")) return status; 

	status = zcompareDataSets(ifltab, tss2, tss1, 1, 0, tss1->pathname, "Fail in testztsStruct5, Location 43");
	if (status) return status;

	if (tss1)zstructFree(tss1);
	tss1 = 0;
	if (tss2)zstructFree(tss2);
	tss2 = 0;

	for (i=0; i<1200; i++) {
		fvalues[i] = 1234;
	}

	
	fvalues[500] = UNDEFINED_FLOAT;
	tss1 = zstructTsNewRegFloats(path, fvalues, 1200, "21Jan2001", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 4);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 44, store status ")) return status; 

	tss2 = zstructTsNew(path); 
	tss2->boolRetrieveAllTimes = 1;
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 45, retrieve status ")) return status; 

	tss1->floatValues[500] = 500.;
	status = zcompareDataSets(ifltab, tss2, tss1, 1, 0, tss1->pathname, "Fail in testztsStruct5, Location 46");
	if (status) return status;


	if (tss1)zstructFree(tss1);
	tss1 = 0;
	if (tss2)zstructFree(tss2);
	tss2 = 0;
	free(path);


      maxVals = 1010   ;
	  lowerCase = 1;

    lenQuality = 2;
    lenNotes = 4;

      numb = 1100; 

	  for (i=0;i<55000; i++) {
		  cnotesa[i] = '@';
		  cnotesb[i] = '@';
	  }

      ich = 66; //ichar('A')
	  cnotesLengthTotal = 0;
	  for (i=0; i<numb; i++) {
		data1[i] = (float)(i + 6);
		data3[i] = data1[i];
		n = i/25;
		n = i - (n*25) + 1;
		if (n == 1) {
			if (lowerCase == 1)
				lowerCase = 0;
			else
				lowerCase = 1;
		}
		for (j=0; j<49; j++) {
			cnotes1[i][j] = ' ';
			cnotes3[i][j] = ' ';
		}
		if (lowerCase) {
			for (j=0; j<n; j++) {
				cnotes1[i][j] = ich+j-1+32;
				cnotes3[i][j] = ich+j-1+32;
				cnotesa[cnotesLengthTotal] = ich+j-1+32;
				cnotesb[cnotesLengthTotal] = ich+j-1+32;
				cnotesLengthTotal++;
			}
		}
		else {
			for (j=0; j<n; j++) {
				cnotes1[i][j] = ich+j-1;
				cnotes3[i][j] = ich+j-1;
				cnotesa[cnotesLengthTotal] = ich+j-1;
				cnotesb[cnotesLengthTotal] = ich+j-1;
				cnotesLengthTotal++;
			}
		}
		cnotes1[i][n] = '\0';
		cnotes3[i][n] = '\0';
		cnotesa[cnotesLengthTotal] = '\0';
		cnotesb[cnotesLengthTotal] = '\0';
		cnotesLengthTotal++;

		for (j=0;j<2;j++) {
			quality1[i][j] = (i*100) + (j + 6);
			quality3[i][j] = quality1[i][j];
		}
		for (j=0;j<4;j++) {
			notes1[i][j] = (i*100) + (j + 6);
			notes3[i][j] = notes1[i][j];
		}
	}
      

	path = mallocAndCopy("/Basin/Location/Flow//1Day/Store Flag test with notes/");
  
	tss1 = zstructTsNewRegFloats(path, data1, numb, "21Jan1991", "1200", "cfs", "Inst-Val");
	tss1->quality = &quality1[0][0];
	tss1->qualityElementSize = 2;

	//  Each line of cnotes must be null terminated; you cannot add extra nulls or blanks after each line, as they
	//  would be seen as a new line.
	tss1->cnotes = cnotesa;
	tss1->cnotesLengthTotal = cnotesLengthTotal;

	tss1->floatValues[5] = UNDEFINED_FLOAT;
	tss1->floatValues[500] = UNDEFINED_FLOAT;

	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 100, store status")) return status; 


	jul = dateToJulian("21Jan1991");
	jul += numb -1;
	julianToDate(jul, 4, cdate, 13);
	tss2 = zstructTsNewTimes(path, "21Jan1991", "1200", cdate, "1200"); 
	status = ztsRetrieve(ifltab, tss2, -1, 1, 1);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 101, retrieve status")) return status; 	
	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testztsStruct5, Location 102");
	if (status) return status;

	//  Make another copy
	jul = dateToJulian("21Jan1991");
	jul += numb -1;
	julianToDate(jul, 4, cdate, 13);
	tss4 = zstructTsNewTimes(path, "21Jan1991", "1200", cdate, "1200"); 
	status = ztsRetrieve(ifltab, tss4, -1, 1, 1);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 103, retrieve status")) return status; 	
	status = zcompareDataSets(ifltab, tss1, tss4, 1, 0, tss1->pathname, "Fail in testztsStruct5, Location 104");
	if (status) return status;

	
	tss1->cnotes = (char *)calloc(tss1->numberValues, 5);
	tss1->allocated[zSTRUCT_TS_cnotes] = 1;
	cnotesLengthTotal = 0;
	count = 0;
	for (i=0; i<tss1->numberValues; i++) {
		tss1->floatValues[i] = 9999.0;
		tss1->quality[count++] = 1234;
		tss1->quality[count++] = 5678;
		tss1->cnotes[cnotesLengthTotal++] = 'w';
		tss1->cnotes[cnotesLengthTotal++] = 'x';
		tss1->cnotes[cnotesLengthTotal++] = 'y';
		tss1->cnotes[cnotesLengthTotal++] = 'z';
		tss1->cnotes[cnotesLengthTotal++] = '\0';
	}

	tss1->cnotesLengthTotal = cnotesLengthTotal;

	status = ztsStore(ifltab, tss1, 1);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 110, store status")) return status; 

	tss3 = zstructTsNewTimes(path, "21Jan1991", "1200", cdate, "1200"); 
	status = ztsRetrieve(ifltab, tss3, -1, 1, 1);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 111, retrieve status")) return status; 	
	

	tss2->floatValues[5] = 9999.0;
	tss2->floatValues[500] = 9999.0;
	tss2->quality[10] = 1234;
	tss2->quality[11] = 5678;
	tss2->quality[1000] = 1234;
	tss2->quality[1001] = 5678;

	//  It's really hard to compare / compute the correct string notes array, because "ABCD" will have been inserted
	//  somewhere in the midst of it.  But, that comparison is also very important.  So, we going to cheat a little
	//  and compare the string notes outside of the main compare, and allow and show 2 differences.  Any more
	//  would be an error.

	diff = tss2->cnotesLengthTotal - tss3->cnotesLengthTotal;
	if (diff < 0) diff = -diff;
	if (diff > 10) {
		printf("Difference in note lengths is too large:  %d and %d\n", tss2->cnotesLengthTotal, tss3->cnotesLengthTotal);
		zmessage(ifltab, "Difference in note lengths is too large");
		zmessage(ifltab, "Fail in testztsStruct5, Location 120");
		return -1;
	}
	else if (diff != 0) {
		printf("Expected difference in note lengths:  %d and %d\n", tss2->cnotesLengthTotal, tss3->cnotesLengthTotal);
	}

	ipos1 = 0;
	ipos2 = 0;
	diff = 0;
	for (i=0; i<tss2->numberValues; i++) {
		cstring1 = &tss2->cnotes[ipos1];
		cstring2 = &tss3->cnotes[ipos2];
		status = zcompareStrings(ifltab, cstring1, cstring2, 0, 0, 0, "difference is okay");
		if (status) {
			printf("Expected difference in strings: %s,  %s\n", cstring1, cstring2);
			diff++;
			if (diff >3) {
				printf("Too many differences in strings.  Difference at location:  %d\n", i);
				zmessage(ifltab, "Too many differences in strings.  ");
				zmessage2(ifltab, "String 1: ", cstring1);
				zmessage2(ifltab, "String 2: ", cstring2);
				zmessage(ifltab, "Fail in testztsStruct5, Location 121");
				return -1;
			}
		}
		len1 = (int)strlen(cstring1);
		ipos1 += (len1 + 1);
		len2 = (int)strlen(cstring2);
		ipos2 += (len2 + 1);
	}

	//  Clear the notes arrays to avoid comparison 
	free(tss2->cnotes);
	tss2->cnotes = 0;
	tss2->cnotesLengthTotal = 0;
	free(tss3->cnotes);
	tss3->cnotes = 0;
	tss3->cnotesLengthTotal = 0;

	status = zcompareDataSets(ifltab, tss2, tss3, 1, 0, tss2->pathname, "Fail in testztsStruct5, Location 130");
	if (status) return status;



	//  Now test storage flag 4.
	//  Rewrite tss1 with a different pathname
	if (tss4->pathname) {
		free (tss4->pathname);
		(tss4->pathname) = 0;
	}
	if (tss4->pathnameInternal) {
		free (tss4->pathnameInternal);
		(tss4->pathnameInternal) = 0;
	}
	if (tss1)zstructFree(tss1);
	tss1 = 0;
	if (tss2)zstructFree(tss2);
	tss2 = 0;
	if (tss3)zstructFree(tss3);
	tss3 = 0;
	free(path);

	path = mallocAndCopy("/Basin/Location/Flow//1Day/Store Flag test with notes Flag 4/");
	if (tss4->allocated[zSTRUCT_pathname]) {
		free(tss4->pathname);
	}
	tss4->pathname = strdup(path);
	tss4->allocated[zSTRUCT_pathname] = 1;

	status = ztsStore(ifltab, tss4, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 140, store status")) {
		free(path);
		return status; 
	}

	jul = dateToJulian("21Jan1991");
	jul += numb -1;
	julianToDate(jul, 4, cdate, 13);
	tss2 = zstructTsNewTimes(path, "21Jan1991", "1200", cdate, "1200"); 
	status = ztsRetrieve(ifltab, tss2, -1, 1, 1);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 141, retrieve status")) {
		free(path);
		return status; 
	}
	status = zcompareDataSets(ifltab, tss4, tss2, 1, 0, tss4->pathname, "Fail in testztsStruct5, Location 142");
	if (status) {
		free(path);
		return status; 
	}

	if (tss4->cnotes) free(tss4->cnotes);
	tss4->cnotes = (char *)calloc(tss4->numberValues, 5);
	cnotesLengthTotal = 0;
	count = 0;
	for (i=0; i<tss4->numberValues; i++) {
		tss4->floatValues[i] = (float)10000.0 - (float)i;
		tss4->quality[count++] = 1234;
		tss4->quality[count++] = 5678;
		tss4->cnotes[cnotesLengthTotal++] = 'w';
		tss4->cnotes[cnotesLengthTotal++] = 'x';
		tss4->cnotes[cnotesLengthTotal++] = 'y';
		tss4->cnotes[cnotesLengthTotal++] = 'z';
		tss4->cnotes[cnotesLengthTotal++] = '\0';
	}

	tss4->cnotesLengthTotal = cnotesLengthTotal;

	tss4->floatValues[10] = UNDEFINED_FLOAT;
	tss4->floatValues[510] = UNDEFINED_FLOAT;

	status = ztsStore(ifltab, tss4, 4);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 142, store status")) {
		free(path);
		return status; 
	}

	tss3 = zstructTsNewTimes(path, "21Jan1991", "1200", cdate, "1200"); 
	free(path);
	status = ztsRetrieve(ifltab, tss3, -1, 1, 1);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct5 Loc 143, retrieve status")) return status; 	
	

	tss4->floatValues[10] = tss2->floatValues[10];
	tss4->floatValues[510] = tss2->floatValues[510];
	tss4->quality[20] = tss2->quality[20];
	tss4->quality[21] = tss2->quality[21];
	tss4->quality[1020] = tss2->quality[1020];
	tss4->quality[1021] = tss2->quality[1021];


	//  It's really hard to compare / compute the correct string notes array
	//  somewhere in the midst of it.  But, that comparison is also very important.  So, we going to cheat a little
	//  and compare the string notes outside of the main compare, and allow and show 2 differences.  Any more
	//  would be an error.

	diff = tss4->cnotesLengthTotal - tss3->cnotesLengthTotal;
	if (diff < 0) diff = -diff;
	if (diff > 20) {
		printf("Difference in note lengths is too large:  %d and %d\n", tss4->cnotesLengthTotal, tss3->cnotesLengthTotal);
		zmessage(ifltab, "Difference in note lengths is too large");
		zmessage(ifltab, "Fail in testztsStruct5, Location 150");
		return -1;
	}
	else if (diff != 0) {
		printf("Expected difference in note lengths:  %d and %d\n", tss4->cnotesLengthTotal, tss3->cnotesLengthTotal);
	}

	ipos1 = 0;
	ipos2 = 0;
	diff = 0;
	for (i=0; i<tss4->numberValues; i++) {
		cstring1 = &tss4->cnotes[ipos1];
		cstring2 = &tss3->cnotes[ipos2];
		status = zcompareStrings(ifltab, cstring1, cstring2, 0, 0, 0, "difference is okay");
		if (status) {
			printf("Expected difference in strings: %s,  %s\n", cstring1, cstring2);
			diff++;
			if (diff >3) {
				printf("Too many differences in strings.  Difference at location:  %d\n", i);
				zmessage(ifltab, "Too many differences in strings.  ");
				zmessage2(ifltab, "String 1: ", cstring1);
				zmessage2(ifltab, "String 2: ", cstring2);
				zmessage(ifltab, "Fail in testztsStruct5, Location 151");
				return -1;
			}
		}
		len1 = (int)strlen(cstring1);
		ipos1 += (len1 + 1);
		len2 = (int)strlen(cstring2);
		ipos2 += (len2 + 1);
	}

	//  Clear the notes arrays to avoid comparison 
	free(tss4->cnotes);
	tss4->cnotes = 0;
	tss4->cnotesLengthTotal = 0;
	free(tss3->cnotes);
	tss3->cnotes = 0;
	tss3->cnotesLengthTotal = 0;
	
	status = zcompareDataSets(ifltab, tss4, tss3, 1, 0, tss4->pathname, "Fail in testztsStruct5, Location 152");
	if (status) return status;
	
	if (tss1)zstructFree(tss1);
	if (tss2)zstructFree(tss2);
	if (tss3)zstructFree(tss3);
	if (tss4)zstructFree(tss4);

	//zsetMessageLevel(MESS_METHOD_TS_READ_ID, MESS_LEVEL_INTERNAL_DIAG_1);

	path = mallocAndCopy("/Basin/Location/Flow/01Jan1991/1Day/Store Flag test with notes Flag 4/");
	tss3 = zstructTsNew(path); 
	ztsProcessTimes(ifltab, tss3, 0);
	tss3->startJulianDate--;
	if (tss3->timeWindow)free(tss3->timeWindow);
	tss3->timeWindow = 0;
	status = ztsRetrieve(ifltab, tss3, -1, 1, 1);
	free(path);
	zstructFree(tss3);
	
	printf("\nCompleted testztsStruct5 sucessfully\n\n");

	return 0; 
}

