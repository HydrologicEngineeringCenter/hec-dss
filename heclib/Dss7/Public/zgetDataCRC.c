#include <stdlib.h>

#include "zdssKeys.h"
#include "hecdss7.h"

/**
*  Function:	zgetDataCRC
*
*  Use:			Public
*
*  Description:	Gets a cycle redundancy check value for the data and header of a record, excluding internal header
*
*  Declaration: unsigned int zgetDataCRC(long long *ifltab, const char *pathname, unsigned int crcIn);
*
*  Parameters:	long long ifltab
*					An integer array dimensioned to int*8 [250] that contains file specific information.
*					This should be considered as a "handle" array and must be passed to any function that accesses that file.*  Parameters:	const char *pathname1:  The first pathname to compare
*
*				const char *pathname:  The pathname of the (single) record to check
*
*				unsigned int crcIn:  Usually 0; an input CRC if several records are being checked together
*
*  Returns:		unsigned int crcValue:	The computed CRC value or 0 if the record was not found or an error occurred.
*
*  Remarks:		Resource intensive; do not use unless you really need, such as transferring across network.
*					DSS Version 7 only
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

/*
 * CRC32 table fill function
 * Copyright (C) 2006 by Rob Sullivan <cogito.ergo.cogito@gmail.com>
 *
 * The following function creates a CRC32 table depending on whether
 * a big-endian (0x04c11db7) or little-endian (0xedb88320) CRC32 is
 * required. Admittedly, there are other CRC32 polynomials floating
 * around, but Busybox doesn't use them.
 *
 * endian = 1: big-endian
 * endian = 0: little-endian
 *
 * Licensed under GPLv2, see file LICENSE in this source tree.
 */

unsigned int* crc32_filltable(unsigned int *crc_table, int endian);
unsigned int  crc32_block_endian1(unsigned int val, const void *buf, unsigned int len, unsigned int *crc_table);
unsigned int  crc32_block_endian0(unsigned int val, const void *buf, unsigned int len, unsigned int *crc_table);

unsigned int zgetDataCRC(long long *ifltab, const char *pathname, unsigned int crcIn)
{
	zStructTransfer* ztransfer;
	unsigned int *crc_table;
	unsigned int crcValue;
	unsigned int len;
	int status;
	int buffer[1]; long long bufferControl[4] ={0,0,0,0};

	int endian = 0;



	if (zgetVersion(ifltab) != 7) {
		return 0;
	}

	crc_table = (unsigned int*)ifltab[zdssKeys.kCRCtable];
	if (!crc_table) {
		crc_table = crc32_filltable(crc_table, endian);
		if (!crc_table) return 0;
		ifltab[zdssKeys.kCRCtable] = (long long)crc_table;
	}

	ztransfer = zstructTransferNew(pathname, 1);
	status = zreadInternal(ifltab, ztransfer, bufferControl, buffer, 0);
	if (status != STATUS_RECORD_FOUND) {
		zstructFree(ztransfer);
		return 0;
	}

	crcValue = crcIn;
	if (ztransfer->values1) {
		len = ztransfer->values1Number * 4;
		if (endian) {
			crcValue = crc32_block_endian1(crcValue, ztransfer->values1, len, crc_table);
		}
		else {
			crcValue = crc32_block_endian0(crcValue, ztransfer->values1, len, crc_table);
		}
	}
	if (ztransfer->values2) {
		len = ztransfer->values2Number * 4;
		if (endian) {
			crcValue = crc32_block_endian1(crcValue, ztransfer->values2, len, crc_table);
		}
		else {
			crcValue = crc32_block_endian0(crcValue, ztransfer->values2, len, crc_table);
		}
	}
	if (ztransfer->values3) {
		len = ztransfer->values1Number * 4;
		if (endian) {
			crcValue = crc32_block_endian1(crcValue, ztransfer->values3, len, crc_table);
		}
		else {
			crcValue = crc32_block_endian0(crcValue, ztransfer->values3, len, crc_table);
		}
	}
	if (ztransfer->header2) {
		len = ztransfer->header2Number * 4;
		if (endian) {
			crcValue = crc32_block_endian1(crcValue, ztransfer->header2, len, crc_table);
		}
		else {
			crcValue = crc32_block_endian0(crcValue, ztransfer->header2, len, crc_table);
		}
	}
	if (ztransfer->userHeader) {
		len = ztransfer->userHeaderNumber * 4;
		if (endian) {
			crcValue = crc32_block_endian1(crcValue, ztransfer->userHeader, len, crc_table);
		}
		else {
			crcValue = crc32_block_endian0(crcValue, ztransfer->userHeader, len, crc_table);
		}
	}


	zstructFree(ztransfer);
	return crcValue;
}

unsigned int* crc32_filltable(unsigned int *crc_table, int endian)
{
	unsigned int polynomial = endian ? 0x04c11db7 : 0xedb88320;
	unsigned int c;
	int i, j;

	if (!crc_table)
		crc_table = (unsigned int *)malloc(256 * sizeof(unsigned int));

	for (i = 0; i < 256; i++) {
		c = endian ? (i << 24) : i;
		for (j = 8; j; j--) {
			if (endian)
				c = (c&0x80000000) ? ((c << 1) ^ polynomial) : (c << 1);
			else
				c = (c&1) ? ((c >> 1) ^ polynomial) : (c >> 1);
		}
		*crc_table++ = c;
	}
	return crc_table - 256;
}

unsigned int  crc32_block_endian1(unsigned int val, const void *buf, unsigned int len, unsigned int *crc_table)
{
	const void *end = (unsigned char*)buf + len;

	while (buf != end) {
		val = (val << 8) ^ crc_table[(val >> 24) ^ *(unsigned char*)buf];
		buf = (unsigned char*)buf + 1;
	}
	return val;
}

unsigned int  crc32_block_endian0(unsigned int val, const void *buf, unsigned int len, unsigned int *crc_table)
{
	const void *end = (unsigned char*)buf + len;

	while (buf != end) {
		val = crc_table[(unsigned char)val ^ *(unsigned char*)buf] ^ (val >> 8);
		buf = (unsigned char*)buf + 1;
	}
	return val;
}

