
#include "hecdss7.h"
#include "TestDssC.h"



void addlocation(zStructLocation *locationStruct1)
{
	int zero = 0;
	int one = 1;
	int i, n, ich;
	int numb;
	int lowerCase;
	char cnotesa[55000];

	numb = 55000;


	//  Now try supplemental info
	ich = 66; //ichar('A')

	lowerCase = 0;
	  for (i=0; i<numb; i++) {
		n = i/25;
		n = i - (n*25) + 1;
		if (n == 1) {
			if (lowerCase == 1)
				lowerCase = 0;
			else
				lowerCase = 1;			
			cnotesa[i] = '\0';
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
}



