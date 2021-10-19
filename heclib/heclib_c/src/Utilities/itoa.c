#include <stdlib.h>
#include <string.h>

#include "hecdssInternal.h"

/**
*  Function:	itoa_hec
*
*  Use:			Semi-private
*
*  Description:	 Convert an integer to ascii.  itoa() is not a standard function, although it should be
*
*  Declaration: void itoa_hec(int num, char* str, int strLength, int base);
*

*
*
*	Author:			Stole directly from K & R
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

/* A utility function to reverse a string  */
void reverse(char str[], int length)
{
	int i, j;
	char c;

	for (i = 0, j = length - 1; i<j; i++, j--) {
		c = str[i];
		str[i] = str[j];
		str[j] = c;
	}
}

// Implementation of itoa_hec()
void itoa_hec(int num, char* str, int strLength, int base)
{

#ifdef _MSC_VER
	_itoa_s(num, str, (size_t)strLength, base);
#else
	int i = 0;
	int isNegative = 0;

	/* Handle 0 explicitely, otherwise empty string is printed for 0 */
	if (num == 0)
	{
		str[i++] = '0';
		str[i] = '\0';
	}

	// In standard itoa_hec(), negative numbers are handled only with
	// base 10. Otherwise numbers are considered unsigned.
	if (num < 0 && base == 10)
	{
		isNegative = 1;
		num = -num;
	}

	// Process individual digits
	while (num != 0)
	{
		int rem = num % base;
		str[i++] = (rem > 9) ? (rem - 10) + 'a' : rem + '0';
		num = num / base;
		if (i >= strLength) break;
	}

	// If number is negative, append '-'
	if (isNegative)
		str[i++] = '-';

	str[i] = '\0'; // Append string terminator

				   // Reverse the string
	reverse(str, i);
#endif
}

