
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
//	long long itemp;

	if (!iarray) return;
	if (numberInts == 0) return;

	unsigned char c1[8];
	unsigned char c2[8];

	numberLongs = ((numberInts - 1) / 2) + 1;
	for (i = 0; i < numberLongs; i++) {
		//emp = iarray[i];
		convertDataType((void *)&iarray[i], (void *)c1, 2, 2);
		*(unsigned char *)c2 = *(unsigned char *)&c1[7];
		*(unsigned char *)&c2[1] = *(unsigned char *)&c1[6];
		*(unsigned char *)&c2[2] = *(unsigned char *)&c1[5];
		*(unsigned char *)&c2[3] = *(unsigned char *)&c1[4];
		*(unsigned char *)&c2[4] = *(unsigned char *)&c1[3];
		*(unsigned char *)&c2[5] = *(unsigned char *)&c1[2];
		*(unsigned char *)&c2[6] = *(unsigned char *)&c1[1];
		*(unsigned char *)&c2[7] = *(unsigned char *)c1;
		convertDataType((void *)c2, (void *)&iarray[i], 2, 2);
	}

}
