#include <stdio.h>
#include <string.h>
#include "heclib.h"

//  Regular-interval time series data with quality and character notes.
int main()
{
	long long ifltab[250];
	zStructTimeSeries *tss1, *tss2;
	float fvalues[200];
	int quality[200];
	char cnotes1[10000];
	int noteCount;

	char cdate[13], ctime[10];
	int valueTime, status, i, j, n;
	int ich;

	//  Open the DSS file; Create if it doesn't exist
	status = zopen(ifltab, "ExampleTimeSeries2.dss");
	if (status != STATUS_OKAY) return status;


	//  Write a regular interval data set.  Gen up the data
	ich = 66; //ichar('A')
	noteCount = 0;
	for (i=0; i<200; i++) {
		fvalues[i] = (float)i;
		quality[i] = i + 10;
		//  Character note
		n = i/25;
		n = i - (n*25) + 1;
		for (j=0; j<n; j++) {
			cnotes1[noteCount++] = ich+j-1+32;
		}
		cnotes1[noteCount++] = '\0';
	}
	tss1 = zstructTsNewRegFloats("/Basin/Location/Flow//1Hour/C with Quality and C notes/", fvalues, 200, "21Jan2001", "1200", "cfs", "Inst-Val");
	tss1->quality = quality;
	tss1->qualityElementSize = 1;
	tss1->cnotes = cnotes1;
	tss1->cnotesLengthTotal = noteCount;
	status = ztsStore(ifltab, tss1, 0);
	zstructFree(tss1);
	if (status != STATUS_OKAY) return status; 

	
	//  Read the data back in
	tss2 = zstructTsNew("/Basin/Location/Flow/01Jan2001/1Hour/C with Quality and C notes/"); 
	//  Remember:  last 3 argumenst: int retrieveFlag, int retrieveDoublesFlag, int boolRetrieveQualityNotes)
	status = ztsRetrieve(ifltab, tss2, -1, 1, 1);
	if (status != STATUS_OKAY) return status; 
	//  Print out.  (Compute time for each ordinate, values are floats)
	valueTime = tss2->startTimeSeconds;
	noteCount = 0;
	for (i=0; i<tss2->numberValues; i++) {		
		getDateAndTime(valueTime, SECOND_GRANULARITY, tss2->startJulianDate, 
						cdate, sizeof(cdate), ctime, sizeof(ctime));
		printf ("Oridnate %d, for %s, %s, value is %f, quality %d, notes ->%s<-\n",i, cdate, ctime, tss2->floatValues[i], tss2->quality[i], &tss2->cnotes[noteCount]);
		noteCount += (int)strlen(&tss2->cnotes[noteCount]) + 1;
		valueTime += 3600;  //  Increment to next hour
	}

	zstructFree(tss2);
	zclose(ifltab);
	return 0; 
}

