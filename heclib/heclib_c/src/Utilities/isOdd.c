

#include "hecdssInternal.h"

/**
*  Function:	isOdd
*
*  Use:			Private (Internal)
*
*  Description:	 A small utility function that determines if the number passed in is odd or even
*
*  Declaration: int isOdd(int number);
*
*  Parameters:	int number - Number to test
*
*  Returns:		booleanOdd
*				0 (false) if number is even
*				1 (true) if number is odd
*

*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int isOdd(int number)
{
	int numb;
	numb = number/2;
	numb *= 2;
	if (numb == number) {
		return 0;
	}
	else {
		return 1;
	}
}

