#include <string.h>

#include "heclib.h"


/**
*  Function:	zcatSortPath
*
*  Use:			Private
*
*  Description:	Writes pathname parts in a specified order and format to a file to sort with
*
*  Declaration: int zcatSortPath(int handle, const char *pathname, size_t pathnameLen, int dataType,
				 int *partMax, int indexNumber);
*
*  Parameters:	int handle
*					The handle of the file to write the formatted pathname to
*
*				const char *pathname
*					The pathname to format and write to the file for sorting.
*
*				size_t pathnameLen
*					The length of the pathname
*
*				int dataType
*					The numeric data type of the pathname, e.g., regular-interval TS is 100, Paired data is 200
*
*				int partMax[6]
*					A 6 element integer array that contains the maximum length for each part in the entire DSS file
*					This information is read from the file header
*
*				int indexNumber
*					The list number of the pathname that is to be written  (pathList[indexNumber]).
*					After this file is sorted, just the index numbers are read, and those are used to
*					create the sorted list.
*
*
*	Returns		0:	Okay
*				Non-zero, an error, usually from the write function
*
*	Remarks:	zcatComparePath() is a companion function
*
*
*
*	See Also:	zcatalogInternal()
*
*
*	Author:			Bill Charley
*	Date:			2016
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


int zcatSortPath(int handle, const char *pathname, size_t pathnameLen, int dataType,
				 int *partMax, int indexNumber)
{

	int i;
	int number;
	int operation;
	int status;

	char cindex[12];
	int len;
	int iorder;
	int order[6];
	char *orderedParts[7];
	int orderedLen[7];

	char *cpart[6];
	int partLen[6];
	int positions[7];
	int columnStart[7];
	int boolLeftJustify[7];
	int jdate;
	int mins;
	char cdate[MAX_PART_SIZE];
	int interval;
	char cinterval[MAX_PART_SIZE];
	char cline[MAX_PATHNAME_SIZE*2];


	number = zpathnamePartPositions (pathname, pathnameLen, positions, 7);
	if (number != 7) {
		return -1;
	}


	//  To sort according to A B C D E F, use
	//                       0 1 2 3 4 5
	for (i=0; i<6; i++) {
		order[i] = i;
		cpart[i] = (char *)&pathname[positions[i]];
		partLen[i] = positions[i+1] - positions[i] - 1;
	}



	//  If data is time series, set the D and E parts to numeric values
	if ((dataType >= DATA_TYPE_RTS) && (dataType < DATA_TYPE_PD)) {

		//  To sort according to A B C F E D, use
		//                       0 1 2 3 4 5
		order[3] = 5;
		order[5] = 3;

		len = stringCopy (cdate, sizeof(cdate), cpart[3], partLen[3]);
		jdate = dateToJulian(cdate);
		//  Valid date?
		if (jdate != UNDEFINED_TIME) {
			//  Need to handle dates prior to 01Jan1900, which are negative
			//  The sort does not treat the "-" sign as negative
			//  So just add a big number to all dates
			jdate += 10000000;
			_snprintf_s(cdate, sizeof(cdate), _TRUNCATE, " %.10d", jdate);
			cpart[3] = cdate;
			partLen[3] = (int)strlen(cdate);
		}
		len = stringCopy (cinterval, sizeof(cinterval), cpart[4], (size_t)partLen[4]);
		operation = EPART_TO_SECONDS;
		status = ztsGetStandardInterval(7, &interval, cinterval, sizeof(cinterval), &operation);
		if (status != STATUS_NOT_OKAY) {
			_snprintf_s(cinterval, sizeof(cinterval), _TRUNCATE, " %.10d", interval);
			cpart[4] = cinterval;
			partLen[4] = (int)strlen(cinterval);
			if (partLen[4] > partMax[4]) {
				partMax[4] = partLen[4];
			}
		}
	}
	else if ((dataType >= 400) && (dataType < 500)) {
		status = spatialDateTime(cpart[3], &jdate, &mins);
		if (status == STATUS_OKAY) {
			jdate += 100000;
			mins /= SECS_IN_1_MINUTE;
			_snprintf_s(cdate, sizeof(cdate), _TRUNCATE, " %.10d %d", jdate, mins);
			cpart[3] = cdate;
			partLen[3] = (int)strlen(cdate);
		}
		//  For gridded data, our sort order is A B C F D E
		//  So switch D and E
		order[4] = 5;
		order[5] = 4;
	}
	else {

	}

	//  Set correct order
	columnStart[0] = 0;
	for (i=0; i<6; i++) {
		iorder = order[i];
		orderedParts[i] = cpart[iorder];
		orderedLen[i] = partLen[iorder];
		columnStart[i+1] = columnStart[i] + partMax[iorder] + 1;
		boolLeftJustify[i] = 1;
	}
	boolLeftJustify[6] = 1;

	_snprintf_s(cindex, sizeof(cindex), _TRUNCATE, " %.10d", indexNumber);
	orderedParts[6] = cindex;
	orderedLen[6] = (int)strlen(cindex);

	len = toFixedFields(cline, sizeof(cline), orderedParts, orderedLen, 7,
				  columnStart, boolLeftJustify);
	status = writeBytes(handle, cline, (size_t)len);

	return status;
}

