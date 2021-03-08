#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "heclib.h"
#include "hecdss7.h"
#include "zStructTimeSeries.h"
#include "hecdssInternal.h"
#include "TestDssC.h"



	//  Regular interval floats, full

int testztsStruct11(long long *ifltab)
{
	zStructTimeSeries *tss1, *tss2;
	int status;
	int numb;
	int zero = 0;


	  float data1[1100], data3[1100];

      int quality1[1100][2], notes1[1100][4];
	  int quality3[1100][2], notes3[1100][4];
	  char cnotes1[1100][50];
	  char cnotes3[1100][50];
	  char cdate[13];

      int i, j, n, ich;
      int lenQuality, lenNotes ;
      int maxVals;
	  int jul;
      int lowerCase;


	  //  This function tests features not supported in DSS-6
	  if (zgetVersion(ifltab) == 6) {
		  return 0;
	  }

      maxVals = 1010   ;
	  lowerCase = 1;

    lenQuality = 2;
    lenNotes = 4;

      
      ich = 66; //ichar('A')
	  for (i=0; i<1100; i++) {
		data1[i] = (float)i;
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
			}
		}
		else {
			for (j=0; j<n; j++) {
				cnotes1[i][j] = ich+j-1;
				cnotes3[i][j] = ich+j-1;
			}
		}
		cnotes1[i][n] = '\0';
		cnotes3[i][n] = '\0';

		for (j=0;j<2;j++) {
			quality1[i][j] = (i*100) + j;
			quality3[i][j] = quality1[i][j];
		}
		for (j=0;j<4;j++) {
			notes1[i][j] = (i*100) + j;
			notes3[i][j] = notes1[i][j];
		}
	}
      
    
	numb = 1100;
	tss1 = zstructTsNewRegFloats("/Basin/Location/Flow//1Day/TSS-Floats-Full/", data1, numb, "21Jan1991", "1200", "cfs", "Inst-Val");
	tss1->quality = &quality1[0][0];
	tss1->qualityElementSize = 2;
	tss1->inotes = &notes1[0][0];
	tss1->inoteElementSize = 4;


//zsetMessageLevel(zmessaging_write_id, MESS_INTERNAL_DIAG_2);
//zsetMessageLevel(zmessaging_read_id, MESS_INTERNAL_DIAG_2);
//zsetMessageLevel(zmessaging_check_id, MESS_INTERNAL_DIAG_2);
	
	status = ztsStore(ifltab, tss1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct11 Loc 1, store status ")) return status;
	
	jul = dateToJulian("21Jan1991");
	jul += numb -1;
	julianToDate(jul, 4, cdate, 13);
	tss2 = zstructTsNewTimes("/Basin/Location/Flow//1Day/TSS-Floats-Full/", "21Jan1991", "1200", cdate, "1200"); 
	//zsetMessageLevel(zmessaging_global_id, MESS_INTERNAL_DIAG_1);

	status = ztsRetrieve(ifltab, tss2, -1, 1, 1);
	if (zcheckStatus(ifltab, status, 1, "Fail in testztsStruct11 Loc 2, retrieve status ")) return status; 

	//  Remove items not supported in DSS-6
	if (zgetVersion(ifltab) == 6) {
		tss1->quality = 0;
		tss1->qualityElementSize = 0;
		tss1->inotes = 0;
		tss1->inoteElementSize = 0;
	}
	status = zcompareDataSets(ifltab, tss1, tss2, 1, 0, tss1->pathname, "Fail in testztsStruct11, Location 3");
	if (status) return status;

	zstructFree(tss1);
	zstructFree(tss2);

	return 0; 
}

