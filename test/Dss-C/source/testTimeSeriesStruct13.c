#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "heclib.h"
#include "hecdss7.h"
#include "zStructTimeSeries.h"
#include "hecdssInternal.h"
#include "TestDssC.h"



	//  Irregular interval doubles, full

int testztsStruct13(long long *ifltab)
{
	zStructTimeSeries *tss1, *tss2;

	int status;
	int numb;
	int zero = 0;


	  double data1[1100], data3[1100];
	  int times[1100];
      int quality1[1100][2], notes1[1100][4];
	  int quality3[1100][2], notes3[1100][4];
	  char cnotes1[1100][50];
	  char cnotes3[1100][50];
	  char cnotesa[55000], cnotesb[55000];
	  char cdate[13];
	  char *cnull=0;

      int i, j, n, ich, one;
      int lenQuality, lenNotes ;
      int maxVals;
	  int jul;
      int lowerCase;
	  int cnotesLengthTotal;


	  //  This function tests features not supported in DSS-6
	  if (zgetVersion(ifltab) == 6) {
		  return 0;
	  }
      maxVals = 1010;
	  lowerCase = 1;
	  one = 1;

    lenQuality = 2;
    lenNotes = 4;

      numb = 1100; 

	  for (i=0;i<55000; i++) {
		  cnotesa[i] = '@';
		  cnotesb[i] = '@';
	  }

	  jul = dateToJulian("21Jan1991");
      ich = 66; //ichar('A')
	  cnotesLengthTotal = 0;
	  for (i=0; i<numb; i++) {
		  //  Noon times
		  times[i] = (jul + i) * MINS_IN_1_DAY + MINS_IN_12_HOURS;
		data1[i] = (double)i;
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
			quality1[i][j] = (i*100) + j;
			quality3[i][j] = quality1[i][j];
		}
		for (j=0;j<4;j++) {
			notes1[i][j] = (i*100) + j;
			notes3[i][j] = notes1[i][j];
		}
	}
      
    
	tss1 = zstructTsNewIrregDoubles("/Basin/Location/Flow//~1Day/TSS-Doubles-Full/", data1, numb, times, MINUTE_GRANULARITY, cnull, "cfs", "Inst-Val");	
	tss1->quality = &quality1[0][0];
	tss1->qualityElementSize = 2;

	//  Each line of cnotes must be null terminated; you cannot add extra nulls or blanks after each line, as they
	//  would be seen as a new line.
	tss1->cnotes = cnotesa;
	tss1->cnotesLengthTotal = cnotesLengthTotal;
	tss1->precision = 2;

	////////////////
	//  Location Data
	//tss1->locationStruct =  zstructLocationNew(0);
	stringCopy (tss1->locationStruct->timeZoneName, sizeof(tss1->locationStruct->timeZoneName), "PST", 3);
	tss1->locationStruct->xOrdinate = -100;
    tss1->locationStruct->yOrdinate = 101;
    tss1->locationStruct->zOrdinate = 102;
    tss1->locationStruct->coordinateSystem = 2;
	tss1->locationStruct->coordinateID = 3;
    tss1->locationStruct->horizontalUnits = 4;
    tss1->locationStruct->horizontalDatum=5;
    tss1->locationStruct->verticalUnits=1;
    tss1->locationStruct->verticalDatum = 2;

	tss1->userHeader = (int *)calloc(6,4);
	tss1->userHeader[0] = 1;
	tss1->userHeader[1] = 2;
	tss1->userHeader[2] = 3;
	tss1->userHeader[3] = 4;
	tss1->userHeader[4] = 5;
	tss1->userHeaderNumber = 5;

	if (bigEndian() && (zgetVersion(ifltab) == 7)) {
		zswitchInts(tss1->userHeader, 6);
	}

	//zset("MLVL", "", 13);
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct13 Loc 1, store status ")) return status;

	if (bigEndian() && (zgetVersion(ifltab) == 7)) {
		zswitchInts(tss1->userHeader, 6);
	}

	jul = dateToJulian("21Jan1991");
	jul += numb -1;
	julianToDate(jul, 4, cdate, 13);
	tss2 = zstructTsNewTimes("/Basin/Location/Flow//~1Day/TSS-Doubles-Full/", "21Jan1991", "1200", cdate, "1200"); 
	status = ztsRetrieve(ifltab, tss2, -1, 2, 1);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct13 Loc 3, retrieve status ")) return status; 

	if (bigEndian() && (zgetVersion(ifltab) == 7)) {
		zswitchInts(tss2->userHeader, 6);
	}

	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testztsStruct13, Location 4");
	if (status) return status;


	free(tss1->userHeader);

	zstructFree(tss1);
	zstructFree(tss2);

	return 0; 
}

