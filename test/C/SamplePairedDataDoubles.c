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
	double fordinates[ROWS_], fvalues[2][ROWS_];
	int status, i, row, column;


	//  Open the DSS file; Create if it doesn't exist
	status = zopen(ifltab, "SamplePairedDataDoubles.dss");
	if (status != STATUS_OKAY) return status;

	//  Write a rating table data set.  Gen up the data
	for (i = 0; i < ROWS_; i++) {
		fordinates[i] = (double)i;
		fvalues[0][i] = (double)(10 * i);
		fvalues[1][i] = (double)(100 * i);
	}
	pd_in = zstructPdNewDoubles("/Basin/Location/Stage-Flow///Test/", fordinates, (double*)fvalues,
		ROWS_, 2, "Feet", "Unt", "cfs", "Unt");
	status = zpdStore(ifltab, pd_in, 0);
	zstructFree(pd_in);
	if (status != STATUS_OKAY) return status;

	//  Now retrieve the data
	pd_out = zstructPdNew("/Basin/Location/Stage-Flow///Test/");
	status = zpdRetrieve(ifltab, pd_out, 2);
	if (status != STATUS_OKAY) return status;

	printPdStruct(pd_out);
	// verify values 

printf("\n check data..");


	for (row = 0; row < pd_out->numberOrdinates; row++) {
		 if( fabs( pd_out->doubleOrdinates[row] - (double) row) > 0.00001)
		 {
			 printf("\n %d expected  %f  actual = %f ",row,(double)row,pd_out->doubleOrdinates[row]);
			 return -1;
		 }

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

