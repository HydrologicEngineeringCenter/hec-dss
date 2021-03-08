#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "heclib.h"
#include "heclibDate.h"

/**
*  Function:	yearMonthDayToDate
*
*  Use:			Public
*
*  Description:	Takes a integer year, month, day date and converts it to a date character string, such as "23Feb2002".
*
*  Declaration: int yearMonthDayToDate(int year, int month, int day, int dateStyle, char *dateString, size_t lenDateString);
*
*  Parameters:	int year
*					The year of the date.  This can either be a two digit or four digit number.
*					For years prior to 2000, a four digit number is required.
*
*				int month
*					The integer month of the date.  1 corresponds to January, 2 to February, etc.
*					This must be a number between 1 and 12.
*
*				int day
*					The integer day of the date. This must be a number between 1 and 31.
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
*
*
*	Returns:	Length (not size) of the destination string.
*				STATUS_NOT_OKAY: Fail (invalid date or date sting too short)
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
*	Remarks:  This is the primary function for converting to a date string; all other similar functions use this.
*
*	See Also:  yearMonthDayToJulian(), dateToYearMonthDay()
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int yearMonthDayToDate(int year, int month, int day, int style, char *dateString, size_t lenDateString)
{

	int i;
	int len;
	int jstyle;
	int ibstyl;
	char charDay[6];
	char charMonth[14];
	char charYear[10];
	char charDate[30];
	char delimiter[2];

	charDate[0] = '\0';
	if ((day < 1) || (day > 31)) return STATUS_NOT_OKAY;
	if ((month < 1) || (month > 12)) return STATUS_NOT_OKAY;

	if (style < 0) {
		if (style == -13) {
			// USGS sytle date yyyy-mm-dd
			intTo2char(day, charDay);
			intTo2char(month, charMonth);
			intTo4char(year, charYear);
			for (i=0; i<4; i++) {charDate[i] = charYear[i];}
			charDate[4] = '-';
			charDate[5] = charMonth[0];
			charDate[6] = charMonth[1];
			charDate[7] = '-';
			charDate[8] = charDay[0];
			charDate[9] = charDay[1];
			charDate[10] = '\0';
			return (int) stringCopy(dateString, lenDateString, charDate, _TRUNCATE);
		}
		if (style < -100) {
			intTo4char(year, charYear);
			style += 100;
		}
		else {
			if (year > 100)
				year = year % 100;
			intTo2char(year, charYear);
		}

		// Numeric Style Date
		if (style == -2 || style == -12) {
			delimiter[0] = '-';
		}
		else {
			delimiter[0] = '/';
		}
		delimiter[1] = '\0';

		if (style < -10) {
			// 08/06/72
			intTo2char(day, charDay);
			intTo2char(month, charMonth);

		}
		else {
			// 8/6/72
			itoa_hec(day, charDay, sizeof(charDay), 10);
			itoa_hec(month, charMonth, sizeof(charMonth), 10);
		}
		//
		stringCopy(charDate, sizeof(charDate), charMonth, _TRUNCATE);
		stringCat(charDate, sizeof(charDate), delimiter, _TRUNCATE);
		stringCat(charDate, sizeof(charDate), charDay, _TRUNCATE);
		stringCat(charDate, sizeof(charDate), delimiter, _TRUNCATE);
		stringCat(charDate, sizeof(charDate), charYear, _TRUNCATE);
		return (int)stringCopy(dateString, lenDateString, charDate, _TRUNCATE);
	}
	else {
	}
		// Character Style Date
		jstyle = style;
		ibstyl = jstyle % 10;
		//
		// Get Month
		stringCopy(charMonth, sizeof(charMonth), LONG_MONTH_NAMES[month-1], _TRUNCATE);
		if (jstyle >= 100) {
			upperCase(charMonth);
			jstyle = jstyle - 100;
		}
		//
		if (ibstyl > 6 || ibstyl == 1) {
			charMonth[3] = ' ';
			charMonth[4] = '\0';
		}
		else if (ibstyl >= 4) {
			charMonth[3] = '\0';
		}
		else {
			len = (int)strlen(charMonth);
			charMonth[len] = ' ';
			len++;
			charMonth[len] = '\0';
		}
		// Get the year
		if (year < 100)	{
//			year = addCentury(year);
		}
		if ((year < 10000) && (year > -1000)) {
			intTo4char(year, charYear);
			if (jstyle >= 10) {
				charYear[0] = charYear[2];
				charYear[1] = charYear[3];
				charYear[2] = '\0';
			}
		}
		else {
			_snprintf_s(charYear, sizeof(charYear), _TRUNCATE, "%d", year);
		}
		// Get the day
		i = ibstyl % 3;
		if (i == 1 && ibstyl > 1) {
			intTo2char(day, charDay);
		}
		else if (i == 2 || ibstyl <= 1) {
			itoa_hec(day, charDay, sizeof(charDay), 10);
		}
		else {
			charDay[0] = '\0';
		}
		// Should a blank follow the day?
		if ((ibstyl == 2) || (ibstyl == 7) || (ibstyl == 8)) {
			len = (int)strlen(charDay);
			charDay[len] = ' ';
			len++;
			charDay[len] = '\0';
		}
		// Now put it all together
		if (ibstyl > 1) {
			if (i != 0) {
				stringCopy(charDate, sizeof(charDate), charDay, _TRUNCATE);
				stringCat(charDate, sizeof(charDate), charMonth, _TRUNCATE);
				stringCat(charDate, sizeof(charDate), charYear, _TRUNCATE);
			}
			else {
				stringCat(charDate, sizeof(charDate), charMonth, _TRUNCATE);
				stringCat(charDate, sizeof(charDate), charDay, _TRUNCATE);
				stringCat(charDate, sizeof(charDate), charYear, _TRUNCATE);
			}
		}
		else {
			stringCopy(charDate, sizeof(charDate), charMonth, _TRUNCATE);
			stringCat(charDate, sizeof(charDate), charDay, _TRUNCATE);
			stringCat(charDate, sizeof(charDate), ", ", _TRUNCATE);
			stringCat(charDate, sizeof(charDate), charYear, _TRUNCATE);
		}
		return (int)stringCopy(dateString, lenDateString, charDate, _TRUNCATE);
}

