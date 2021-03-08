#include "stdio.h"
#include "string.h"
#include "math.h"

#include "heclib.h"
#include "hecdss7.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "TestDssC.h"


void printvals(char *c1, char *c2);


int comparePDs (long long *ifltab, zStructPairedData *pds1, zStructPairedData *pds2, const char *str)
{
	int status;
	int one = 1;
	int two = 2;
	int ierror;
	int numberValues;
	char mess[150];
	int numberColumns;
	int numberRows;


	copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 1, numberCurves ", str); 
	checknumbers_(&pds1->numberCurves, &pds2->numberCurves, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 2, numberOrdinates ", str);  
	checknumbers_(&pds1->numberOrdinates, &pds2->numberOrdinates, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 3, startingCurve ", str);
	if (pds1->startingCurve > 0) {
		checknumbers_(&pds1->startingCurve, &pds2->startingCurve, mess, &status, strlen(mess));
	} 
	else {
		numberColumns = 1;
		checknumbers_(&numberColumns, &pds2->startingCurve, mess, &status, strlen(mess));
	}
	if (status != STATUS_OKAY) return status;

	copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 4, endingCurve ", str);  
	if (pds1->endingCurve > 0) {
		checknumbers_(&pds1->endingCurve, &pds2->endingCurve, mess, &status, strlen(mess));
	}
	else {
		checknumbers_(&pds1->numberCurves, &pds2->endingCurve, mess, &status, strlen(mess));
	}
	if (status != STATUS_OKAY) return status;

	copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 5, startingOrdinate ", str);  
	if (pds1->startingOrdinate > 0) {
		checknumbers_(&pds1->startingOrdinate, &pds2->startingOrdinate, mess, &status, strlen(mess));
	}
	else {
		numberRows = 1;
		checknumbers_(&numberRows, &pds2->startingOrdinate, mess, &status, strlen(mess));
	}
	if (status != STATUS_OKAY) return status;

	copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 6, endingOrdinate ", str); 
	if (pds1->endingOrdinate > 0) {
		checknumbers_(&pds1->endingOrdinate, &pds2->endingOrdinate, mess, &status, strlen(mess));
	}
	else {
		checknumbers_(&pds1->numberOrdinates, &pds2->endingOrdinate, mess, &status, strlen(mess));
	}
	if (status != STATUS_OKAY) return status;

	if (pds1->endingOrdinate > 0) {
		numberRows = pds1->endingOrdinate - pds1->endingOrdinate + 1;
	}
	else {
		numberRows = pds1->numberOrdinates;
	}
	if (pds1->endingCurve > 0) {
		numberColumns = pds1->startingCurve - pds1->endingCurve + 1;
	}
	else {
		numberColumns = pds1->numberCurves;
	}

	if (pds1->doubleOrdinates && pds2->doubleOrdinates) {
		copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 23, Ordinates ", str);  
		checkdoubles_(pds1->doubleOrdinates, pds2->doubleOrdinates, &numberRows, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;

		numberValues = numberRows * numberColumns;
		copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 24, doubleValues ", str);  
		checkdoubles_(pds1->doubleValues, pds2->doubleValues, &numberValues, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	else if (pds1->floatOrdinates && pds2->floatOrdinates) {
		copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 25, float Ordinates ", str);  
		checkfloats_(pds1->floatOrdinates, pds2->floatOrdinates, &numberRows, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;

		numberValues = numberRows * numberColumns;
		copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 26, floatValues ", str);  
		checkfloats_(pds1->floatValues, pds2->floatValues, &numberValues, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	else {
		copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 27, ordinates array miss-match ", str); 
		zmessage(ifltab, mess); 
		return -1;
	}

	if (pds1->unitsIndependent && pds2->unitsIndependent) {
		copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 30, unitsIndependent ", str);  
		checkstring_(pds1->unitsIndependent, pds2->unitsIndependent, mess, &status, strlen(pds1->unitsIndependent), strlen(pds2->unitsIndependent), strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
		
	if (pds1->typeIndependent && pds2->typeIndependent) {
		copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 31, typeIndependent ", str);  
		checkstring_(pds1->typeIndependent, pds2->typeIndependent, mess, &status, strlen(pds1->typeIndependent), strlen(pds2->typeIndependent), strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	
	if (pds1->unitsDependent && pds2->unitsDependent) {
		copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 32, unitsDependent ", str);  
		checkstring_(pds1->unitsDependent, pds2->unitsDependent, mess, &status, strlen(pds1->unitsDependent), strlen(pds2->unitsDependent), strlen(mess));
		if (status != STATUS_OKAY) return status;
	}

	if (pds1->typeDependent && pds2->typeDependent) {
		copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 33, typeDependent ", str);  
		checkstring_(pds1->typeDependent, pds2->typeDependent, mess, &status, strlen(pds1->typeDependent), strlen(pds2->typeDependent), strlen(mess));
		if (status != STATUS_OKAY) return status;
	}

	if (pds1->timeZoneName && pds2->timeZoneName) {
		copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 34,  timeZoneName ", str); 
		checkstring_(pds1->timeZoneName, pds2->timeZoneName, mess, &status, strlen(pds1->timeZoneName), strlen(pds2->timeZoneName), strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	else if (!(!pds1->timeZoneName && !pds2->timeZoneName)) {
		//  Okay to have a blank and a null
		ierror = 0;
		//  One or the other has to be true (and the other false)
		if (pds1->timeZoneName) {
			ierror = (int)strlen(pds1->timeZoneName);
		}
		else if (pds2->timeZoneName) {
			ierror = (int)strlen(pds2->timeZoneName);
		}
		if (ierror) {
			copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 35,  timeZoneName  miss-match ", str); 
			zmessage(ifltab, mess); 
			return -1;
		}
	}

	copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 36, labelsLength ", str); 
	checknumbers_(&pds1->labelsLength, &pds2->labelsLength, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;


	if (pds1->labels && pds2->labels) {
		copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 37,  labels ", str); 
		checkstring_(pds1->labels, pds2->labels, mess, &status, strlen(pds1->labels), strlen(pds2->labels), strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	else {
		if (!(!pds1->labels && !pds2->labels)) {
			copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 38,  labels  miss-match ", str); 
			zmessage(ifltab, mess); 
			return -1;
		}
	}

	copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 20, xprecision ", str); 
	checknumbers_(&pds1->xprecision, &pds2->xprecision, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 20a, yprecision ", str); 
	checknumbers_(&pds1->yprecision, &pds2->yprecision, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 21, boolIndependentIsXaxis ", str); 
	checknumbers_(&pds1->boolIndependentIsXaxis, &pds2->boolIndependentIsXaxis, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 30, userHeaderNumber ", str); 
	checknumbers_(&pds1->userHeaderNumber, &pds2->userHeaderNumber, mess, &status, strlen(mess));
	if (status != STATUS_OKAY) return status;

	if (pds1->userHeader && pds2->userHeader) {
		copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 31, userHeader ", str); 
		checkints_(pds1->userHeader, pds2->userHeader, &one, &pds1->userHeaderNumber, mess, &status, strlen(mess));
		if (status != STATUS_OKAY) return status;
	}
	else {
		if (!(!pds1->userHeader && !pds2->userHeader)) {
			copyTwoStrings(mess, sizeof(mess), "comparePDs Loc 32, userHeader array miss-match ", str); 
			zmessage(ifltab, mess); 
			return -1;
		}
	}	
	
	return 0;
}


