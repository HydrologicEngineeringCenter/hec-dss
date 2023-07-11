#include <string.h>

#include "hecdssInternal.h"
#include "heclibDate.h"


/**
*  Function:	getDateTimeString
*
*  Use:			Public
*
*  Description:	Returns a character date and character time from a julian date and time in seconds past midnight.
*
*  Declaration: void getDateTimeString (int julian, char *dateString, size_t sizeofDateString, int dateStyle,
					    int secondsPastMidnight, char *timeString, size_t sizeofTimeString, int timeStyle);
*
*  Parameters:	int julian
*					Date in days since 01Jan1900, with 01Jan1900 being day 1 (31Dec1899 is day 0)
*					This is the standard day count used throughout DSS and can be negative (or large).
*					An undefined day is the number "-2147483647".  Note, 0 (zero) is valid day and used a lot!
*
*				char *dateString (output)
*					A String to containt the current date.  Should be declared large enough to hold style (below).
*					Examples:  "05Apr1976" (Style 4), "6/2/1985" (Style -101)
*
*				size_t sizeofDateString
*					The size of dateString passed in.  Should include space for null terminator.
*
*				int dateStyle
*					The style code of how the data should be returned, as given below in the table.
*					dateStyle "4" is the standard date style used by DSS (e.g., "02Jun1985").
*
*				int secondsPastMidnight
*					The current time of day in seconds past midnight.
*					"1" is one second past midnight.  "0" doesn't exist, as midnight belongs to the end
*					of the day, by convention, and midnight is the number SECS_IN_1_DAY.
*					secondsPastMidnight varies from 1 to SECS_IN_1_DAY.
*
*				char *timeString (output)
*					A string to contain the complete current time.  Must be dimensioned to hold string with null term.
*					Example time string: "08:23:12".
*
*				size_t sizeofTimeString
*					The size of timeString.  Should be at least 13.
*
*				int timeStyle
*					The style code of how the time should be returned, as given below in the table.
*
*
*	Returns:	None
*
*	Date Style Codes:
*
*      Style  Example      Style  Example     Style  Example       Style  Example
*
*         0:  June 2, 1985   10:  June 2, 85   100:  JUNE 2, 1985   110:  JUNE 2, 85
*         1:  Jun 2, 1985    11:  Jun 2, 85    101:  JUN 2, 1985    111:  JUN 2, 85
*         2:  2 June 1985    12:  2 June 85    102:  2 JUNE 1985    112:  2 JUN 85
*         3:  June 1985      13:  June 85      103:  JUNE 1985      113:  JUNE 85
*         4:  02Jun1985      14:  02Jun85      104:  02JUN1985      114:  02JUN85
*         5:  2Jun1985       15:  2Jun85       105:  2JUN1985       115:  02JUN85
*         6:  Jun1985        16:  Jun85        106:  JUN1985        116:  JUN85
*         7:  02 Jun 1985    17:  02 Jun 85    107:  02 JUN 1985    117:  02 JUN 85
*         8:  2 Jun 1985     18:  2 Jun 85     108:  2 JUN 1985     118:  2 JUN 85
*         9:  Jun 1985       19:  Jun 85       109:  JUN 1985       119:  JUN 85
*     Numeric Date Styles:
*          -1:  6/2/85     -101:  6/2/1985
*          -2:  6-2-85     -102:  6-2-1985
*         -11: 06/02/85    -111:  06/02/1985
*         -12: 06-02-85    -112:  06-02-1985
*         -13: 1985-06-02
*
*
* Time Style Codes:
*
*      Style  Example
*
*          0:  0830
*          1:  08:30
*          2:  08:30:43
*
*
*
* See Also:
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void getDateTimeString (int julian, char *dateString, size_t sizeofDateString, int dateStyle,
					    int secondsPastMidnight, char *timeString, size_t sizeofTimeString, int timeStyle)
{
	julianToDate(julian, dateStyle, dateString, sizeofDateString);
	secondsToTimeString(secondsPastMidnight, 0, timeStyle, timeString, sizeofTimeString);
}

