#include <string.h>

#include "heclib.h"


/**
*  Function:	getCurrentDateTimeString
*
*  Use:			Public
*
*  Description:	Returns the current local system time as a character date and character time.
*					Local system time means that the time zone is included, but daylight time is not.
*
*  Declaration: void getCurrentDateTimeString (char *dateString, size_t sizeofDateString, int style,
*					char timeString[13], size_t sizeofTimeString);
*
*  Parameters:	char *dateString (output)
*					A String to containt the current date.  Should be declared large enough to hold style (below).
*					Examples:  "05Apr1976" (Style 4), "6/2/1985" (Style -101)
*
*				size_t sizeofDateString
*					The size of dateString.  Should include space for null terminator.
*
*				int style
*					The style code of how the data should be returned, as given below in the table.
*					Style "4" is the standard date style used by DSS (e.g., "02Jun1985")
*
*				char timeString[13] (output)
*					A string to contain the complete current time, including mili-seconds.  Must be dimensioned to 13.
*					Example time string: "08:23:12.084"
*
*				size_t sizeofTimeString
*					The size of timeString.  Should be at least 13.
*
*
*	Returns:	None
*
*	Style Codes:
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
*	Remarks:	style "4" is the standard date style used by DSS.
*
* See Also: getCurrentDateTimeString()
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

void getCurrentDateTimeString (char *dateString, size_t sizeofDateString, int style,
							   char timeString[13], size_t sizeofTimeString)
{
	int julian;
	int secondsPastMidnight;
	int mills;


    getCurrentDateTime (&julian, &secondsPastMidnight, &mills);
	julianToDate(julian, style, dateString, sizeofDateString);
	secondsToTimeString(secondsPastMidnight, mills, 3, timeString, sizeofTimeString);

}

void getcurrentdatetimestring_(char *dateString, int *sizeofDateString, int *style,
							   char *timeString, int *sizeofTimeString, size_t lenDate, size_t lenTime)
{
	char date[30];
	char time[15];

	getCurrentDateTimeString (date, sizeof(date), *style, time, sizeof(time));

	stringCToFort(dateString, (size_t)*sizeofDateString, date);
	stringCToFort(timeString, (size_t)*sizeofTimeString, time);
}

