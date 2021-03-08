#include <string.h>

#include "heclib7.h"
#include "hecdssInternal.h"

/**
*  Function:
*
*  Use:			Semi-private
*
*  Description:	 A small utility function that converts two ints (integer 4) into a long (integer 8)
*
*  Declaration: long long i4toi8(int int4a, int int4b);
*
*  Parameters:
*				int int4a - The integer to occupy the first part of the long word
*				int int4b - The integer to occupy the second part of the long word
*
*  Returns:		long long i8 - an int 8 with the first int 4 in the first part of the word, and
*					the second in the second part.
*
*  See Also:	i8toi4()
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/
int getLastInt(const char *cline)
{
	int i;
	int start;
	int end;
	int numb;

	numb = -1;
	end = (int)strlen(cline) -1;
	if (end < 0) return end;

	//  Remove trailing blanks
	if (cline[end] == ' ') {
		end--;
		for (i=end; i>0; i--) {
			if (cline[i] != ' ') {
				end = i+1;
				break;
			}
		}
	}
	if (end < 0) return -1;
	start = end -1;
	for (i=start; i>0; i--) {
		if (cline[i] == ' ') {
			start = i+1;
			break;
		}
	}
	//  Number begin at start of line?
	if (start == (end-1)) {
		start = 0;
	}

	numb = findInt((const char *)&cline[start], &i, &end);
	return numb;
}

