#include <string.h>
#include <stdio.h>

#include "hecdssInternal.h"

/**
*  Function:	longWithCommas
*
*  Use:			Semi-private
*
*  Description:	 A small utility function for printing a long with commas (e.g.  "12,345,678")
*
*  Declaration: int longWithCommas(char *str, size_t strSize, long long number);
*
*  Parameters:
*				char *str - A character string to print the number with commas to.  Be sure this is large enough.
*				size_t strSize - The size of str
*				long long number - the number to print
*
*  Returns:		The length of the string, or -1 if str is not large enough
*	*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int longWithCommas(char *str, size_t strSize, long long number)
{
	char temp[30];
	int i;
	int count;
	int ipos;
	size_t len;
	int boolNegative;

	if (number < 0) {
		boolNegative = 1;
		number = -number;
	}
	else {
		boolNegative = 0;
	}

	_snprintf_s(str, strSize, _TRUNCATE, "%lld", number);
	len = strlen(str);
	count = 0;
	ipos = 0;
	for (i=(int)(len-1); i>=0; i--) {
		temp[count++] = str[i];
		if (count == strSize) return -1;
		ipos++;
		if ((ipos == 3) && (i != 0)) {
			temp[count++] = ',';
			ipos = 0;
			if (count == strSize) return -1;
		}
	}

	//  Now it has commas, but is inversed.  Right side it.
	if (boolNegative) count++;
	ipos = 0;
	for (i=(count-1); i>=0; i--) {
		str[i] = temp[ipos++];
	}
	if (boolNegative) str[0] = '-';
	str[count] = '\0';
	return count;
}

