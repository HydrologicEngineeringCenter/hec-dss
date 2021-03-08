#include <stdio.h>
#include <string.h>
#include <math.h>// for fabs()
#include "heclib.h"

void printPdStruct(zStructPairedData *pds);

#define ROWS_  9 // number of rows

int main()
{
	long long ifltab[250];
	zStructPairedData *pd_in, *pd_out;
	float fordinates[ROWS_], fvalues[2][ROWS_];
	int status, i, row, column;
	for (int i = 0; i < 250; i++)
		ifltab[i] = 0;

	//getchar();
	zsetMessageLevel(999, 999);
	//  Open the DSS file; Create if it doesn't exist
	status = zopen6(ifltab, "SamplePairedData6.dss");
	printf("\nreturned from zopen %d ", status);
	if (status != STATUS_OKAY)
	{
		printf("\nError: failed to open file %d", status);
		return status;
	}

	//  Write a rating table data set.  Gen up the data
	for (i = 0; i < ROWS_; i++) {
		fordinates[i] = (float)i;
		fvalues[0][i] = (float)(10 * i);
		fvalues[1][i] = (float)(100 * i);
	}
	pd_in = zstructPdNewFloats("/Basin/Location/Stage-Flow///Test/", fordinates, (float *)fvalues,
		ROWS_, 2, "Feet", "Unt", "cfs", "Unt");
	printf("\nafter zstructPdNewFloats");
	status = zpdStore(ifltab, pd_in, 0);
	printf("\n after zpdStore %d", status);
	zstructFree(pd_in);
	printf("\n after zstructFree %d", status);
	if (status != STATUS_OKAY)
	{
		return status;
	}
	//  Now retrieve the data
	pd_out = zstructPdNew("/Basin/Location/Stage-Flow///Test/");
	printf("\nzstructPdNew");
	status = zpdRetrieve(ifltab, pd_out, 1);
	printf("\nzpdRetrieve");

	if (status != STATUS_OKAY)
	{
		printf("\nError after zpdRetrieve %d", status);
		return status;
	}

	printPdStruct(pd_out);
	// verify values 

	for (row = 0; row < pd_out->numberOrdinates; row++) {
		 if( fabs( pd_out->floatOrdinates[row] - (float) row) > 0.00001)
			 return -1;

	}
	for (row = 0; row < pd_out->numberOrdinates; row++) {
		//for (column = 0; column < pd_out->numberCurves; column++) {
			//pos = row + column * pds->numberOrdinates;
		if(    fabs(pd_out->floatValues[row ] - (float)(10 * row) ) > 0.000001
			|| fabs(pd_out->floatValues[row +  pd_out->numberOrdinates] - (float)(100 * row)) > 0.000001
				)
					return -1;
	//}
   }

	
	zstructFree(pd_out);
	if (status != STATUS_OKAY) return status;

	zclose(ifltab);
	return 0;
}


 


void printPdStruct(zStructPairedData *pds)
{
	 
	int row;
	int column;
	int pos;

	//  Need to compute correct position within the array

	printf("\nRow \t  Ordinate \t");
	for (column = 0; column < pds->numberCurves; column++) {
		printf(" Curve %d \t", column);
	}
	printf("\n");

	for (row = 0; row < pds->numberOrdinates; row++) {
		printf(" %d \t",  row);
		if (pds->floatOrdinates) {
			printf(" %f \t", pds->floatOrdinates[row]);
		}
		else {
			printf(" %f \t", pds->doubleOrdinates[row]);
		}
		for (column = 0; column < pds->numberCurves; column++) {
			pos = row  + column * pds->numberOrdinates;
			if (pds->floatValues) {
				printf(" %f \t", pds->floatValues[pos]);
			}
			else {
				printf(" %f \t", pds->doubleValues[pos]);
			}
		}
		printf("\n");
	}
}

