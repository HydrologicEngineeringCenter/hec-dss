#include <ctype.h>
#include <string.h>

#include "hecdssInternal.h"

/**
*  Function:	findInt
*
*  Use:			public
*
*  Description:	 Finds the first int in a string and returns the int value, location and number of digits.
*
*  Declaration: int findInt(const char *string, int *index, int *numberDigits);
*
*  Parameters:	const char *string
*					A null-terminated string that contains (somewhere in it) an integer.
*
*				int *index (output)
*					Returns the character position of the first digit.
*
*				int *numberDigits (output)
*					Returns the number of digits in that int.
*					If an int is not found, this is returned 0.
*
*  Returns:		int - the integer value found
*		 			(see *numberDigits if no int is found
*
*  Remarks:		The int can be anywhere within the string
*				An ASCII dependent function.
*
*  Example:		" temperature: -15 degrees"
*					returns -15, with index pointing to "-" and numberDigits set to 3.
*
*				i = findInt(" temperature: -15 degrees", &index, &numberDigits);
*				//           012345678901234
*					i: -15
*					index: 14
*					numberDigits: 3
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int findInt(const char *string, int *index, int *numberDigits)
{
	int i;
	int len;
	int idx;
	int ndigits;
	int value;
	int digit;

	len = (int)strlen(string);
	idx = 0;
	ndigits = 0;
	for (i=0; i<len; i++) {
		if ((string[i] > 47) && (string[i] < 58)) {
			//  Found int char!
			if (ndigits == 0) {
				idx = i;
			}
			ndigits++;
		}
		else {
			if (ndigits > 0) {
				break;
			}
		}
	}
	//  Didn't find an int?
	if (ndigits == 0) {
		*numberDigits = 0;
		return 0;
	}

	//  Now compute the number
	value = 0;
	for (i=0; i<ndigits; i++) {
		digit = (int)string[idx+i] - 48;  //  48 is ascii "0"
		value = (value * 10) + digit;
	}
	//  We have the number... is it negative?
	*index = idx;
	if (idx > 0) {
		idx--;
		if (string[idx] == '-') {
			value = -value;
			*index = idx;
			ndigits++;
		}
	}
	*numberDigits = ndigits;
	return value;
}

