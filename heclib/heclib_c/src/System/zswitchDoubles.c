
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "hecdssInternal.h"


/**
*  Function:	zswitchDoubles
*
*  Use:			Private (Internal)
*
*  Description:	Switch double words in long words for converting between big and little endian.  
*
*  Declaration: void zswitchDoubles(int *iarray, int numberArray, int lengthEachValue, int numberValues, int boolStoring, int numberTimes);
*
*  Parameters:	int *iarray
*					The array to switch words in.  iarray will contain times, data, quality, notes
*
*				int numberArray
*					The number of ints in iarray to switch.  This is usually the position of the last data value
*
*				int lengthEachValue
*					The length of each data value, 1 for floats, 2 for doubles
*
*				int numberValues
*					The number of time or data values (or just data values if no times)
*
*				int boolStoring
*					boolean 1 if storing, 0 if retrieving
*
*				int numberTimes
*					If iarray does not start with times, then this is 0.  If it does, then the number of times.
*
*
*	Remarks:	Since the native word size for DSS is 64 bit, long swapping is done
*					at the low level write / read functions.  For DSS, the native 
*					endianness  is little, because of the prominent use of the PC
*					As a result of this, ints and floats are in incorrect positions for 
*					every long word.
*
*					If the number of values is odd, then the data will be one int off because of the time ints
*					 We solve this by two int swaps
*
*					Loc A
*					t7, t8
*					t9, d1a
*					d1b, d2a
*					d2b, d3a
*					goes to  //  Loc B
*					t7, t8
*					t9, d1b,
*					d1a, d2b
*					d2a, d3b
*					goes to  //  Loc C
*					t7, t8
*					t9, d1b,
*					d2b, d1a
*					d3b, d2a
*					goes to  //  Loc D
*					t8, t7
*					d1b, t9,
*					d2b, d1a
*					d3b, d2a
*					bytes are swapped and array written out
*					On PC, the array will be read in as original:
*					t7, t8
*					t9, d1a
*					d1b, d2a
*					d2b, d3a
*
*
*	Author:			Bill Charley
*	Date:			2017
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


void zswitchDoubles(int *iarray, int numberArray, int lengthEachValue, int boolStoring, int numberTimes) {
	int number;

	if (!iarray) return;
	if (numberArray == 0) return;
	/*
	printf("In zswitchDoubles, numberArray = %d\n", numberArray);
	printf("lengthEachValue = %d\n", lengthEachValue);
	printf("numberTimes = %d\n", numberTimes);
	printf("numberValues = %d\n", numberValues);
	*/

	if (lengthEachValue == 1) {
		zswitchInts(iarray, numberArray);
	}
	else {
		//  Doubles, swap times (first part of iarray, up to numberValues
		if (numberTimes) {
			if (isOdd(numberTimes)) {
				if (boolStoring) {
					
					number = numberArray - numberTimes;
					//  Loc A
					zswitchInts(&iarray[numberTimes], number);
					//  Loc B
					zswitchInts(&iarray[numberTimes + 1], number - 1);
					//  Loc C	swap times			
					zswitchInts(iarray, numberTimes);
					//  Loc D	
				}
				else {
					//  Retrieving - doing the opposite					
					number = numberArray - numberTimes;
					zswitchInts(iarray, numberArray);
					zswitchInts(&iarray[numberTimes], number);
				}
			}
			else {
				//  Just Swap times
				zswitchInts(iarray, numberTimes);
			}
		}
	}

}