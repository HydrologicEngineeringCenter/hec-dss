#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "heclib.h"
#include "hecdss7.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "TestDssC.h"



int testlocation(long long *ifltab)
{
	int status;
	int zero = 0;
	int one = 1;
	int i, n, ich;
	int numb;
	int lowerCase;
	char cnotesa[55000];

	zStructLocation *locationStruct1;
	zStructLocation *locationStruct2;


	//  A version 7 function only
	if (zgetVersion(ifltab) != 7) return 0;



	numb = 100;

		////////////////
	//  Location Data
	locationStruct1 =  zstructLocationNew("/Basin/Location/Flow//~1Day/Location Test/");

	stringCopy (locationStruct1->timeZoneName, sizeof(locationStruct1->timeZoneName), "PST", 3);
	locationStruct1->xOrdinate = -100.;
    locationStruct1->yOrdinate = 101.;
    locationStruct1->zOrdinate = 102.;
    locationStruct1->coordinateSystem = 2;
	locationStruct1->coordinateID = 3;
    locationStruct1->horizontalUnits = 4;
    locationStruct1->horizontalDatum=5;
    locationStruct1->verticalUnits=1;
    locationStruct1->verticalDatum = 2;

	status = zlocationStore(ifltab, locationStruct1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testlocation, store status")) return status;

	locationStruct2 =  zstructLocationNew("/Basin/Location/Flow//~1Day/Location Test/");
	status = zlocationRetrieve(ifltab, locationStruct2);
	if (zcheckStatus(ifltab, status, 1, "Fail in testlocation, retrieve status")) return status;

	status = zcompareDataSets(ifltab, locationStruct1, locationStruct2, 1, 0, locationStruct1->pathname, "testlocation");

	zstructFree(locationStruct1);
	zstructFree(locationStruct2);

	//  Now try supplemental info
	ich = 66; //ichar('A')

	lowerCase = 0;
	cnotesa[0] = 'a';
	for (i=0; i<numb; i++) {
		n = i/25;
		n = i - (n*25) + 1;
		if (n == 1) {
			if (lowerCase == 1)
				lowerCase = 0;
			else
				lowerCase = 1;
			if (i > 2)
				cnotesa[i] = '\n';
		}
		else {
			if (lowerCase) {
				cnotesa[i] = ich+n-1+32;				
			}
			else {
				cnotesa[i] = ich+n-1;
			}
		}
	}
	cnotesa[numb-1] = '\0';


	locationStruct1 =  zstructLocationNew("/Basin/Location with Supp/Flow//~1Day/Location Test/");

	stringCopy (locationStruct1->timeZoneName, sizeof(locationStruct1->timeZoneName), "Los Angeles", 11);
	locationStruct1->xOrdinate = -100.;
    locationStruct1->yOrdinate = 101.;
    locationStruct1->zOrdinate = 102.;
    locationStruct1->coordinateSystem = 2;
	locationStruct1->coordinateID = 3;
    locationStruct1->horizontalUnits = 4;
    locationStruct1->horizontalDatum=5;
    locationStruct1->verticalUnits=1;
    locationStruct1->verticalDatum = 2;
	locationStruct1->supplemental = cnotesa;

	status = zlocationStore(ifltab, locationStruct1, 0);
	if (zcheckStatus(ifltab, status, 1, "Fail in testlocation, store status")) return status;

	locationStruct2 =  zstructLocationNew("/Basin/Location with Supp/Flow//~1Day/Location Test/");
	status = zlocationRetrieve(ifltab, locationStruct2);
	if (zcheckStatus(ifltab, status, 1, "Fail in testlocation, retrieve status")) return status;

	status = zcompareDataSets(ifltab, locationStruct1, locationStruct2, 1, 0, locationStruct1->pathname, "testlocation");

	zstructFree(locationStruct1);
	zstructFree(locationStruct2);

	if (zcheckStatus(ifltab, status, 1, "Fail in testlocation, zcompareDataSets")) return status;

	printf("Completed testlocation  successfully!\n");

	return 0; 
}


