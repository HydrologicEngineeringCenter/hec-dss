
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "hecdssInternal.h"


/**
*  Function:	zswitchInts
*
*  Use:			Private (Internal)
*
*  Description:	Switch int words in long words for converting between big and little endian.  
*
*  Declaration: void zswitchInts(int *iarray, int numberInts);
*
*  Parameters:	int *iarray
*					The array to switch words in
*
*				int numberInts
*					The number of ints to switch.   This must be an even number.
*
*
*	Remarks:	Since the native word size for DSS is 64 bit, long swapping is done
*					at the low level write / read functions.  For DSS, the native 
*					endianness  is little, because of the prominent use of the PC
*					As a result of this, ints and floats are in incorrect positions for 
*					every long word.
*
*				For Little endian, int (and float) words are stored like this:
*					1000 2000 3000 4000
*				For big endian, like this:
*					0001 0002 0003 0004
*				Byte swapping for longs from big endian to little endian gives us this
*					2000 1000 4000 3000
*				zswitchInts switches ints so they are like this:
*					1000 2000 3000 4000
*
*
*				Endianness is how low level bytes are ordered, and is a
*					remnant of the history of the chip
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

void zswitchInts(int *iarray, int numberInts)
{
	int numberLongs;
	int i;

	int ipos;
	unsigned int itemp;
	unsigned int *uarray;

	if (!iarray) return;
	if (numberInts == 0) return;

	uarray = (unsigned int*)iarray;

	numberLongs = ((numberInts - 1) / 2) + 1;
	for (i = 0; i < numberLongs; i++) {
		ipos = i * 2;
		itemp = uarray[ipos];
		uarray[ipos] = uarray[ipos + 1];
		uarray[ipos + 1] = itemp;
	}

/*
	unsigned char c1[8];
	unsigned char c2[8];

	numberLongs = ((numberInts - 1) / 2) + 1;
	for (i = 0; i < numberLongs; i++) {
		convertDataType((void *)&iarray[i*2], (void *)c1, 2, 2);
		*(unsigned char *)c2 = *(unsigned char *)&c1[4];
		*(unsigned char *)&c2[1] = *(unsigned char *)&c1[5];
		*(unsigned char *)&c2[2] = *(unsigned char *)&c1[6];
		*(unsigned char *)&c2[3] = *(unsigned char *)&c1[7];
		*(unsigned char *)&c2[4] = *(unsigned char *)&c1[0];
		*(unsigned char *)&c2[5] = *(unsigned char *)&c1[1];
		*(unsigned char *)&c2[6] = *(unsigned char *)&c1[2];
		*(unsigned char *)&c2[7] = *(unsigned char *)&c1[3];
		convertDataType((void *)c2, (void *)&iarray[i*2], 2, 2);
	}
*/
}
