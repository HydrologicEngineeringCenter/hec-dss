#include "hecdssInternal.h"


/**
*  Function:	getEndian
*
*  Use:			Private (Internal)
*
*  Description:	Gets the endian-ness for this OS / Chip
*
*  Declaration: int getEndian();
*
*
*	Returns:
*				Zero (0) for Little Endian
*				One (1) for Big Endian
*
*	Remarks:	Endianness is how low level bytes are ordered, and is a
*					reminiant of the history of the chip
*					Big endian is for Sun Solairs / RISC chips.  Bytes are stored in this order:
*						12345678
*					Little endian is for Windows / Intel chips.  Bytes are stored in this order:
*						87654321
*
*
*	Author:			Bill Charley
*	Date:			2017
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int getEndian()
{
    short int i = 0x1;
    char *c = (char*)&i;
    return (c[0] != 1);
}

void getendian_(int *iend)
{
	*iend = getEndian();
}

