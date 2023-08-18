
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "hecdssInternal.h"


/**
*  Function:	zswap
*
*  Use:			Private (Internal)
*
*  Description:	Swap bytes for converting between big and little endian.  
*					DSS files are little endian (more PC users than Unix users)
*
*  Declaration: void zswap(long long *iarray, int numberInts);
*
*  Parameters:	long long *iarray
*					The array to swap bytes in
*
*				int numberInts
*					The number of ints to swap (not longs).  (The functions that eventually call
*					this are passing ints, but working with 64 bit words.)
*
*
*	Remarks:	Since the native word size for DSS is 64 bit, long swapping is done
*					at the low level write / read functions.
*					int and char swapping is done with higher level functions, where
*					you know the values are ints or chars.  These are swapped, so that	
*					low level long swap is correct.  (i.e., chars are swapped, then swapped back)
*					charInt takes care of character swapping.
*
*
*				Endianness is how low level bytes are ordered, and is a
*					reminiant of the history of the chip
*					Big endian is for Sun Solairs / RISC chips.  
*					ints are stored in this order:
*						0001 0002 0003 0004
*					long longs:
*						0000001 00000002 00000003 00000004
*					chars:
*						abcdefgh
*					Little endian is for Windows / Intel chips.  
*					ints are stored in this order:
*						1000 2000 3000 4000
*					long longs:
*						1000000 20000000 30000000 40000000
*					chars:
*						abcdefgh
*
*
*	Author:			Bill Charley
*	Date:			2017
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void zswap(long long *iarray, int numberInts)
{
	int numberLongs;
	int i;
	long long itemp;

	if (!iarray) return;
	if (numberInts == 0) return;

	unsigned char *c1;
	unsigned char *c2;

	numberLongs = ((numberInts - 1) / 2) + 1;
	for (i = 0; i < numberLongs; i++) {
		itemp = iarray[i];
		c1 = (unsigned long*)&itemp;
		c2 = (unsigned long*)&iarray[i];
		c2[0] = c1[7];
		c2[1] = c1[6];
		c2[2] = c1[5];
		c2[3] = c1[4];
		c2[4] = c1[3];
		c2[5] = c1[2];
		c2[6] = c1[1];
		c2[7] = c1[0];
	}

}
