#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "heclib7.h"

int numberBytesInDoubleCharArray(const char **charArray, int numberRows, int numberColumns, int *expandedSize)
{
	//  Counts the number of bytes in numberStrings null terminated strings in charArray.
	//  Any character less than 0 is considered filler and ignored.
	//  If max is set (> 0), then limit that number of bytes
	int i;
	int j;
	int ipos;
	int numberBytes;
	int len;
	int *maxColumnSize;
	const char *cstring;

	numberBytes = 0;
	maxColumnSize = (int *)calloc((size_t)numberColumns, WORD_SIZE);
	for (i=0; i<numberRows; i++) {
		for (j=0; j<numberColumns; j++) {
			ipos = (i * numberColumns) + j;
			cstring = charArray[ipos];
			len = (int)strlen(cstring) + 1;
			numberBytes += len;
			if (len > maxColumnSize[j]) {
				maxColumnSize[j] = len;
			}
		}
	}
	*expandedSize = 0;
	for (j=0; j<numberColumns; j++) {
		*expandedSize += maxColumnSize[j];
	}
	*expandedSize *= numberRows;
	if (maxColumnSize) {
		free(maxColumnSize);
	}

	return numberBytes;
}

int compressCharArray(const char **charArrayIn, char *charArrayOut, int numberRows, int numberColumns, int max)
{
	//  Counts the number of bytes in numberStrings null terminated strings in charArray.
	//  Any character less than 0 is considered filler and ignored.
	//  If max is set (> 0), then limit that number of bytes
	int i;
	int j;
	int ipos;
	int numberBytes;
	int len;
	const char *cstring;


	numberBytes = 0;
	for (i=0; i<numberRows; i++) {
		for (j=0; j<numberColumns; j++) {
			ipos = (i * numberColumns) + j;
			cstring = charArrayIn[ipos];
			len = (int)strlen(cstring) + 1;

			if ((numberBytes + len) > max) {
			}
			stringCopy(&charArrayOut[numberBytes], (max - numberBytes), cstring, len);
			numberBytes += len;
		}
	}
	return numberBytes;
}


int buildTablePointerArray(char *charArrayIn, char **tablePointerArrayOut, int numberRows, int numberColumns, int max)
{

	int i;
	int j;
	int ipos;
	int numberBytes;
	int len;
	char *cstring;


	numberBytes = 0;
	for (i=0; i<numberRows; i++) {
		for (j=0; j<numberColumns; j++) {
			cstring = &charArrayIn[numberBytes];
			ipos = (i * numberColumns) + j;
			tablePointerArrayOut[ipos] = cstring;
			len = (int)strlen(cstring) + 1;
			numberBytes += len;
			if (numberBytes > max) {
				//  FIX ME
			}
		}
	}
	return numberBytes;
}




