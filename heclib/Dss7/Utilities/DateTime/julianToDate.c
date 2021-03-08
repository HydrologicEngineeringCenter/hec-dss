#include "heclib7.h"
#include "hecdss7.h"
#include "heclibDate.h"

/**
*  Function:	julianToDate
*
*  Use:			Public
*
*  Description:	Takes a Julian day date and converts it to a date character string, such as "23Feb2002".
*
*  Declaration: int julianToDate(int julianDate, int dateStyle, char *dateString, size_t sizeofDateString);
*
*  Parameters:	int julianDate
*					Julian date to convert, in days since 01Jan1900, with 01Jan1900 being day 1 (31Dec1899 is day 0)
*					This is the standard day count used throughout DSS and can be negative (or large).
*					An undefined day is the number "-2147483647".  Note, 0 (zero) is valid day and used a lot!
*
*				int dateStyle
*					The style code of how the data should be returned, as given below in the table.
*					Style "4" is the standard date style used by DSS (e.g., "02Jun1985")
*
*				char *dateString (output)
*					A String to containt the current date.  Should be declared large enough to hold style (below).
*					Examples:  "05Apr1976" (Style 4), "6/2/1985" (Style -101)
*
*				size_t sizeofDateString
*					The size of dateString.  Should include space for null terminator.
*
*	Returns:	STATUS_OKAY:  Success
*				STATUS_NOT_OKAY: Fail (undefined Julian or date sting too short)
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
*
* See Also: julianToYearMonthDay(), yearMonthDayToDate(), secondsToTimeString()
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int julianToDate(int julianDate, int dateStyle, char *dateString, size_t sizeofDateString)
{

	int year;
	int month;
	int day;
	int status;

	if (julianDate == UNDEFINED_TIME) {
		stringCopy (dateString, sizeofDateString, "undefined\0", (size_t)10);
		return STATUS_NOT_OKAY;
	}

	//  Convert to year, month, day, as there is a function to convert to our format
	julianToYearMonthDay (julianDate, &year, &month, &day);
	status = yearMonthDayToDate(year, month, day, dateStyle, dateString, sizeofDateString);
	return status;
}

