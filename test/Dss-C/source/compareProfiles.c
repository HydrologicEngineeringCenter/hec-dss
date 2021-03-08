#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

#include "heclib.h"
#include "hecdss7.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "TestDssC.h"

int compareDifferent(double *doubles, float *floats, int number, const char *str);


int compareProfiles (long long *ifltab, zStructTimeSeries* tss1, zStructTimeSeries* tss2, const char *str)
{
	int status;
	int one = 1;
	int two = 2;
	int i;
	char mess[150];


	//  When comparing profiles, we'll have different data types - not an error.
	
	status = compareTss(ifltab, tss1, tss2, str);
	if (status != STATUS_OKAY) return status;

	copyTwoStrings(mess, sizeof(mess), "compareProfiles Loc 1, numberDepths ", str); 
	checknumbers_(&tss1->profileDepthsNumber, &tss2->profileDepthsNumber, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	copyTwoStrings(mess, sizeof(mess), "compareProfiles Loc 2, numberTimes ", str); 
	checknumbers_(&tss1->numberValues, &tss2->numberValues, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;


	if (tss1->floatProfileDepths && tss2->floatProfileDepths) {
		copyTwoStrings(mess, sizeof(mess), "compareProfiles Loc 3, floatDepths ", str); 
		checkfloats_(tss1->floatProfileDepths, tss2->floatProfileDepths, &tss2->profileDepthsNumber, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	else if (tss1->doubleProfileDepths && tss2->doubleProfileDepths) {
		copyTwoStrings(mess, sizeof(mess), "compareProfiles Loc 3a, doubleDepths ", str); 
		checkdoubles_(tss1->doubleProfileDepths, tss2->doubleProfileDepths, &tss2->profileDepthsNumber, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	else if (tss1->floatProfileDepths && tss2->doubleProfileDepths) {
		copyTwoStrings(mess, sizeof(mess), "compareProfiles Loc 3b, Depths ", str); 
		status = compareDifferent(tss2->doubleProfileDepths, tss1->floatProfileDepths, tss2->profileDepthsNumber, mess);
		if (status != STATUS_OKAY) return status;
	}
	else if (tss2->floatProfileDepths && tss1->doubleProfileDepths) {
		copyTwoStrings(mess, sizeof(mess), "compareProfiles Loc 3c, Depths ", str); 
		status = compareDifferent(tss1->doubleProfileDepths, tss2->floatProfileDepths, tss2->profileDepthsNumber, mess);
		if (status != STATUS_OKAY) return status;
	}
	else {
		copyTwoStrings(mess, sizeof(mess), "compareProfiles Loc 4, floatValues array miss-match ", str); 
		zmessage(ifltab, mess); 
		return -1;
	}

	i = tss1->profileDepthsNumber * tss1->numberValues;
	if (tss1->floatProfileValues && tss2->floatProfileValues) {
		copyTwoStrings(mess, sizeof(mess), "compareProfiles Loc 5, ProfileValues ", str); 
		checkfloats_(tss1->floatProfileValues, tss2->floatProfileValues, &i, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	else if (tss1->doubleProfileValues && tss2->doubleProfileValues) {
		copyTwoStrings(mess, sizeof(mess), "compareProfiles Loc 5a, ProfileValues ", str); 
		checkdoubles_(tss1->doubleProfileValues, tss2->doubleProfileValues, &i, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	else if (tss1->floatProfileValues && tss2->doubleProfileValues) {
		copyTwoStrings(mess, sizeof(mess), "compareProfiles Loc 5b, ProfileValues ", str); 
		status = compareDifferent(tss2->doubleProfileValues, tss1->floatProfileValues, i, mess);
		if (status != STATUS_OKAY) return status;
	}
	else if (tss2->floatProfileValues && tss1->doubleProfileValues) {
		copyTwoStrings(mess, sizeof(mess), "compareProfiles Loc 5c, ProfileValues ", str); 
		status = compareDifferent(tss1->doubleProfileValues, tss2->floatProfileValues, i, mess);
		if (status != STATUS_OKAY) return status;
	}
	else {
		if (!(!tss1->doubleProfileValues && !tss2->doubleProfileValues)) {
			copyTwoStrings(mess, sizeof(mess), "compareProfiles Loc 10, doubleValues array miss-match ", str); 
			zmessage(ifltab, mess); 
			return -1;
		}
	}

	

	if (tss1->unitsProfileDepths && tss2->unitsProfileDepths) {
		copyTwoStrings(mess, sizeof(mess), "compareProfiles Loc 9, unitsProfileDepths ", str); 
		checkstring_(tss1->unitsProfileDepths, tss2->unitsProfileDepths, mess, &status, strlen(tss1->unitsProfileDepths), strlen(tss2->unitsProfileDepths), strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	else {
		if (!(!tss1->unitsProfileDepths && !tss2->unitsProfileDepths)) {
			copyTwoStrings(mess, sizeof(mess), "compareProfiles Loc 10, unitsProfileDepths miss-match ", str); 
			zmessage(ifltab, mess); 
			return -1;
		}
	}

	if (tss1->unitsProfileValues && tss2->unitsProfileValues) {
		copyTwoStrings(mess, sizeof(mess), "compareProfiles Loc 11, unitsProfileValues ", str); 
		checkstring_(tss1->unitsProfileValues, tss2->unitsProfileValues, mess, &status, strlen(tss1->unitsProfileValues), strlen(tss2->unitsProfileValues), strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	else {
		if (!(!tss1->unitsProfileValues && !tss2->unitsProfileValues)) {
			copyTwoStrings(mess, sizeof(mess), "compareProfiles Loc 12, unitsProfileValues miss-match ", str); 
			zmessage(ifltab, mess); 
			return -1;
		}
	}

	
	return 0;
}

int compareDifferent(double *doubles, float *floats, int number, const char *str)
{
	//  Compare floats, as going from float to double might have round differences
	float *dfloats;
	int status;

	dfloats = (float *)calloc(number, 4);
	convertDataArray((void*)doubles, (void*)dfloats, number, 2, 1);
	checkfloats_((void*)dfloats, (void*)floats, &number, str, &status, strlen(str));
	free(dfloats);
	return status;
}


