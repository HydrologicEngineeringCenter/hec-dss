#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "heclib.h"
#include "hecdss7.h"
#include "zStructTimeSeries.h"
#include "hecdssInternal.h"
#include "TestDssC.h"



	//  Regular interval floats, full

int testztsStruct12(long long *ifltab)
{
	zStructTimeSeries *tss1, *tss2;

	int status;
	int numb;
	int zero = 0;


	  double data1[1100], data3[1100];
      int quality1[1100][2], notes1[1100][4];
	  int quality3[1100][2], notes3[1100][4];
	  char cnotes1[1100][50];
	  char cnotes3[1100][50];
	  char cnotesa[55000], cnotesb[55000];
	  char cdate[13];

      int i, j, n, ich;
      int lenQuality, lenNotes ;
      int maxVals;
	  int jul;
      int lowerCase;
	  int cnotesLengthTotal;

	  //  This function tests features not supported in DSS-6
	  if (zgetVersion(ifltab) == 6) {
		  return 0;
	  }

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
		data1[i] = (double)(i + 6);
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
      
    //zset("MLVL", "", 15);
	//zsetMessageLevel(zmessaging_tswrite_ID, MESS_INTERNAL_DIAG_2);

	/////////////////////////////////////////////////////////////////
	////  Bill's test
	tss1 = zstructTsNewRegDoubles("/Bills/One value/Note Test//1Day/TSS-Doubles-Full/",  data1, 5, "21Jan1991", "1200", "cfs", "Inst-Val");
	//tss1->quality = &quality1[0][0];
	//tss1->qualityElementSize = 2;

	//  Each line of cnotes must be null terminated; you cannot add extra nulls or blanks after each line, as they
	//  would be seen as a new line.
	//tss1->cnotes = cnotesa;
	//tss1->cnotesLengthTotal = cnotesLengthTotal;
//zset("mlvl", "", 15);
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct12 Loc 0a, store status")) return status;
	zstructFree(tss1);
	//zstructFree(tss1);
//zset("mlvl", "", 15);
	tss1 = zstructTsNewRegDoubles("/Bills/One value/Note Test//1Day/TSS-Doubles-Full/",  data1, 5, "21Jan1991", "1200", "cfs", "Inst-Val");
	//tss1->quality = &quality1[0][0];
	//tss1->qualityElementSize = 2;

	//  Each line of cnotes must be null terminated; you cannot add extra nulls or blanks after each line, as they
	//  would be seen as a new line.
	tss1->cnotes = cnotesa;
	tss1->cnotesLengthTotal = cnotesLengthTotal;


	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct12 Loc 1, store status")) return status; 
	zstructFree(tss1);

	/////////////////////////////////////////////////////////////////
	tss1 = zstructTsNewRegDoubles("/Basin/Location/Flow//1Day/TSS-Doubles-Full/", data1, numb, "21Jan1991", "1200", "cfs", "Inst-Val");
	tss1->quality = &quality1[0][0];
	tss1->qualityElementSize = 2;

	//  Each line of cnotes must be null terminated; you cannot add extra nulls or blanks after each line, as they
	//  would be seen as a new line.
	tss1->cnotes = cnotesa;
	tss1->cnotesLengthTotal = cnotesLengthTotal;

	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct12 Loc 2, store status")) return status; 


	jul = dateToJulian("21Jan1991");
	jul += numb -1;
	julianToDate(jul, 4, cdate, 13);
	tss2 = zstructTsNewTimes("/Basin/Location/Flow//1Day/TSS-Doubles-Full/", "21Jan1991", "1200", cdate, "1200"); 
	status = ztsRetrieve(ifltab, tss2, -1, 1, 1);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct12 Loc 3, retrieve status")) return status; 
	//  Since we are asking for floats and doubles are in tss1, the compare would fail...
	//  So we need to convert the doubles to floats before comparing
	tss2->doubleValues = (double *)calloc(tss2->numberValues, 8);
	tss2->allocated[zSTRUCT_TS_doubleValues] = 1;
	convertDataArray((void *)tss2->floatValues, (void *)tss2->doubleValues, tss2->numberValues, 1, 2);
	free(tss2->floatValues);
	tss2->floatValues = 0;
	tss2->allocated[zSTRUCT_TS_floatValues] = 0;
	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testztsStruct12, Location 4");
	if (status) return status;

	zstructFree(tss1);
	zstructFree(tss2);

	return 0; 
}

