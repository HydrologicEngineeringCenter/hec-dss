#include "stdio.h"
#include "string.h"
#include "math.h"

#include "heclib.h"
#include "hecdss7.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "TestDssC.h"

int compareDifferent(double *doubles, float *floats, int number, const char *str);

void copyTwoStrings(char *mess, int sizeofMess, const char* str1, const char* str2)
{
	stringCopy(mess, sizeofMess, str1, strlen(str1));
	stringCat(mess, sizeofMess, str2, strlen(str2));
}

int compareTss (long long *ifltab, zStructTimeSeries* tss1, zStructTimeSeries* tss2, const char *str)
{
	int status;
	int one = 1;
	int two = 2;
	int i;
	int exact = 0;
	char mess[150];


	copyTwoStrings(mess, sizeof(mess), "compareTss Loc 1, number values ", str); 
	checknumbers_(&tss1->numberValues, &tss2->numberValues, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	if (tss1->times && tss2->times) {
		copyTwoStrings(mess, sizeof(mess), "compareTss Loc 2, times ", str); 
		zcompareTimeArrays(ifltab, tss1, tss2, 1, 0, mess);
		if (status != STATUS_OKAY) return status;
	}
	else {
		if (!(!tss1->times && !tss2->times)) {
			//  This is okay now, one might have times and the other not.
			//copyTwoStrings(mess, sizeof(mess), "compareTss Loc 2, time array miss-match ", str); 
			//zmessage(ifltab, mess); 
			//return -1;
		}
	}

	if (exact) {
		copyTwoStrings(mess, sizeof(mess), "compareTss Loc 3a, julianBaseDate ", str); 
		checknumbers_(&tss1->julianBaseDate, &tss2->julianBaseDate, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	
		copyTwoStrings(mess, sizeof(mess), "compareTss Loc 3b, startTimeSeconds ", str); 
		checknumbers_(&tss1->startTimeSeconds, &tss2->startTimeSeconds, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	
		copyTwoStrings(mess, sizeof(mess), "compareTss Loc 3c, endTimeSeconds ", str); 
		checknumbers_(&tss1->endTimeSeconds, &tss2->endTimeSeconds, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	
		copyTwoStrings(mess, sizeof(mess), "compareTss Loc 3d, timeGranularitySeconds ", str); 
		if (tss1->timeGranularitySeconds && tss2->timeGranularitySeconds) {
			checknumbers_(&tss1->timeGranularitySeconds, &tss2->timeGranularitySeconds, mess, &status, strlen(mess));
			if (status != STATUS_OKAY) return status;
		}
	
		copyTwoStrings(mess, sizeof(mess), "compareTss Loc 3e, timeOffset ", str); 
		checknumbers_(&tss1->timeOffsetSeconds, &tss2->timeOffsetSeconds, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}

	if (tss1->floatValues && tss2->floatValues) {
		copyTwoStrings(mess, sizeof(mess), "compareTss Loc 4, floats ", str); 
		checkfloats_(tss1->floatValues, tss2->floatValues, &tss2->numberValues, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	else if (tss1->doubleValues && tss2->doubleValues) {
		copyTwoStrings(mess, sizeof(mess), "compareTss Loc 4a, doubleValues ", str); 
		checkdoubles_(tss1->doubleValues, tss2->doubleValues, &tss2->numberValues, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	} 
	else if (tss1->doubleValues && tss2->floatValues) {
		copyTwoStrings(mess, sizeof(mess), "compareTss Loc 4b, Values ", str); 
		status = compareDifferent(tss1->doubleValues, tss2->floatValues, tss2->numberValues, mess);
		if (status != STATUS_OKAY) return status;	
	}
	else if (tss2->doubleValues && tss1->floatValues) {
		copyTwoStrings(mess, sizeof(mess), "compareTss Loc 4c, Values ", str); 
		status = compareDifferent(tss2->doubleValues, tss1->floatValues, tss2->numberValues, mess);
		if (status != STATUS_OKAY) return status;	
	}
	else {
	}


	if (tss1->units && tss2->units) {
		copyTwoStrings(mess, sizeof(mess), "compareTss Loc 5, units ", str); 
		checkstring_(tss1->units, tss2->units, mess, &status, strlen(tss1->units), strlen(tss2->units), strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	else {
		status = zcompareStrings(ifltab, tss1->units, tss2->units, 0, 1, 1, " compareTss Loc 5, units miss-match");
		if (status != STATUS_OKAY) return status;
	}

	if (tss1->type && tss2->type) {
		copyTwoStrings(mess, sizeof(mess), "compareTss Loc 5, type ", str); 
		checkstring_(tss1->type, tss2->type, mess, &status, strlen(tss1->type), strlen(tss2->type), strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	else {
		status = zcompareStrings(ifltab, tss1->type, tss2->type, 0, 1, 1, " compareTss Loc 5, type miss-match");
		if (status != STATUS_OKAY) return status;	
	}

	copyTwoStrings(mess, sizeof(mess), "compareTss Loc 6, precision ", str); 
	checknumbers_(&tss1->precision, &tss2->precision, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;


	copyTwoStrings(mess, sizeof(mess), "compareTss Loc 7, qualityElementSize ", str); 
	checknumbers_(&tss1->qualityElementSize, &tss2->qualityElementSize, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	if (tss1->quality && tss2->quality) {
		copyTwoStrings(mess, sizeof(mess), "compareTss Loc 8, quality ", str); 
		checkints_(tss1->quality, tss2->quality, &tss2->qualityElementSize, &tss2->numberValues, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	else {
		if (!(!tss1->quality && !tss2->quality)) {
			copyTwoStrings(mess, sizeof(mess), "compareTss Loc 8, quality array miss-match ", str); 
			zmessage(ifltab, mess); 
			return -1;
		}
	}

	copyTwoStrings(mess, sizeof(mess), "compareTss Loc 9, inoteElementSize ", str); 
	checknumbers_(&tss1->inoteElementSize, &tss2->inoteElementSize, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	if (tss1->inotes && tss2->inotes) {
		copyTwoStrings(mess, sizeof(mess), "compareTss Loc 10, inotes ", str); 
		checkints_(tss1->inotes, tss2->inotes, &tss2->inoteElementSize, &tss2->numberValues, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	else {
		if (!(!tss1->inotes && !tss2->inotes)) {
			copyTwoStrings(mess, sizeof(mess), "compareTss Loc 10, inotes array miss-match ", str); 
			zmessage(ifltab, mess); 
			return -1;
		}
	}

	copyTwoStrings(mess, sizeof(mess), "compareTss Loc 11, cnotesLengthTotal ", str); 
	checknumbers_(&tss1->cnotesLengthTotal, &tss2->cnotesLengthTotal, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	if (tss1->cnotes && tss2->cnotes) {
		copyTwoStrings(mess, sizeof(mess), "compareTss Loc 12, cnotes ", str); 
		for (i=0; i<tss2->cnotesLengthTotal; i++) {
		if (tss1->cnotes[i] != tss2->cnotes[i]) {
			zmessage(ifltab, mess);
			return -1;
		}
	}
		checkstring_(tss1->type, tss2->type, mess, &status, strlen(tss1->type), strlen(tss2->type), strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	else {
		if (!(!tss1->cnotes && !tss2->cnotes)) {
			copyTwoStrings(mess, sizeof(mess), "compareTss Loc 12, cnotes miss-match ", str); 
			zmessage(ifltab, mess); 
			return -1;
		}
	}
	return 0;
}


