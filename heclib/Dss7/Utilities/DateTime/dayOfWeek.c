#include "heclibDate.h"

/**
*  Function:	dayOfWeek
*
*  Use:			public
*
*  Description:	 Returns the day of the week, where Sunday = 1, Saturday = 7 (sabbath, or seventh)
*
*  Declaration: int dayOfWeek(int julian)
*
*  Parameters:	int julian
*					The Julian date to use
*
*  Returns:		dayOfWeek
*		 			The day of the week, with Sunday being 1.
*
*	Author:			Originally written in Fortran by Art Pabst, cira 1980
*					Translated to C by Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int dayOfWeek(int julian)
{
	int iday;

	iday = (julian % 7) + 1;
	return iday;
}

