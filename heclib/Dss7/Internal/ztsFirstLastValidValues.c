#include "hecdssInternal.h"
#include "heclib.h"

/**
*  Function:	ztsFirstLastValidValues
*
*  Use:			Private
*
*  Description:	 Determine the position of the first and last valid (non-missing) values for the arrays 
*					used in regular-interval time-series.
*
*  Declaration: int ztsFirstLastValidValues(void *values, int valueElementSize, 
*									  int profileDepthsNumber, int numberValues,
*									  int *quality, int sizeQuality,
*									  int *notes, int sizeNotes,
*									  char *cnotes, int cnotesLengthTotal,
*									  int *firstValid, int *lastValid);
*
*  Parameters:	void *values
*					The values array to search.  Can be either floats or doubles.
*
*				int valueElementSize
*					The size of an individual values element; 1 for floats, 2 for doubles
*
*				int profileDepthsNumber
*					If a profile dataset, this is the number of values (depths) for each time.
*					For non profile data, set to 0 (zero)
*
*				int numberValues
*					The number of data in the values array.  If profile data, this is the 
*						number of times (excluding depths number.)
*
*				int *quality
*					If quality is used, this is the quality array to search.  Zero (0) quality will be 
*					consider missing, if the associated value is missing.
*
*				int sizeQuality
*					The size of each quality element, in integer words.  Set to zero for no quality.
*
*				int *notes
*					If integer notes are used, this is the note array to search.  Zero (0) will be 
*					consider missing, if the associated value is missing.
*
*				int sizeNotes
*					The size of each note element, in words.  Set to zero for no integer notes.
*
*				char *cnotes
*					If characters notes are used, this is the note array to search.  A null character ('\0') will be 
*					consider missing, if the associated value is missing.
*
*				int cnotesLengthTotal
*					The total length for the character notes array.  Set to zero for no notes.
*
*				int *firstValid (output)
*					Returns the position of the first valid value, quality or note.  The very first position is zero.
*					If all the values are missing, then this is set to -1 and the function returns true (1)
*
*				int *lastValid (output)
*					Returns the position of the last valid value, quality or note.  The very last position is (numberValues - 1).
*					If all the values are missing, then this is set to -1 and the function returns true (1)
*
*
*
*  Returns:		booleanAllMissing
*					0 (false) if valid data found
*					1 (true)  if no valid data found
*	
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/


//  Private functions only used here

int ztsFirstLastValidFloat(float *values, int profileDepthsNumber, int numberValues,
							 int *quality, int sizeQuality,
							 int *notes, int sizeNotes,
							 char *cnotes, int cnotesLengthTotal,
						     int *firstValid, int *lastValid);

int ztsFirstLastValidDouble(double *values, int profileDepthsNumber, int numberValues,
							 int *quality, int sizeQuality,
							 int *notes, int sizeNotes,
							 char *cnotes, int cnotesLengthTotal,
						     int *firstValid, int *lastValid);


int ztsFirstLastValidValues(void *values, int valueElementSize, 
							int profileDepthsNumber, int numberValues,
							   int *quality, int sizeQuality,
							   int *notes, int sizeNotes,
							   char *cnotes, int cnotesLengthTotal,
							   int *firstValid, int *lastValid)
{


	if (valueElementSize == 1) {
		return ztsFirstLastValidFloat((void *)values, profileDepthsNumber, numberValues, 
										quality, sizeQuality,
										notes, sizeNotes,
										cnotes, cnotesLengthTotal,
										firstValid, lastValid);
	}
	else if (valueElementSize == 2) {
		return ztsFirstLastValidDouble((void *)values, profileDepthsNumber, numberValues,
										 quality, sizeQuality,
										 notes, sizeNotes,
										 cnotes, cnotesLengthTotal,
										 firstValid, lastValid);
	}
	else {
		*firstValid = 0;
		*lastValid = numberValues - 1;
		return 0;
	}
}

int ztsFirstLastValidFloat(float *values, int profileDepthsNumber, int numberValues,
							 int *quality, int sizeQuality,
							 int *notes, int sizeNotes,
							 char *cnotes, int cnotesLengthTotal,
						     int *firstValid, int *lastValid)
{
	int i;
	int j;
	int ipos;

	//  Look for first non missing
	*firstValid = -1;
	for (i=0; i<numberValues; i++) {
		if (profileDepthsNumber == 0) {
			if (!zisMissingFloat(values[i])) {
				*firstValid = i;
				break;
			}
		}
		else {
			for (j=0; j<profileDepthsNumber; j++) {
				ipos = (i * profileDepthsNumber) + j;
				if (!zisMissingFloat(values[ipos])) {
					*firstValid = i;
					break;
				}
			}
			if (*firstValid != -1) 
				break;
		}
		if (sizeQuality > 0) {
			for (j=0; j<sizeQuality; j++) {
				ipos = (i * sizeQuality) + j;
				if (quality[ipos] != 0) {
					if (*firstValid == -1) {
						*firstValid = i;
					}
					break;
				}
			}
		}
		if (sizeNotes > 0) {
			for (j=0; j<sizeNotes; j++) {
				ipos = (i * sizeNotes) + j;
				if (notes[ipos] != 0) {
					if (*firstValid == -1) {
						*firstValid = i;
					}
					break;
				}
			}
		}
		if (cnotesLengthTotal > 0) {
			//  A missing or non-valid C note is '\0'.  Anything else is valid
			if (i < cnotesLengthTotal) {
				if (cnotes[i] != '\0') {
					if (*firstValid == -1) {
						*firstValid = i;
					}
					break;
				}
			}
		}
		if (*firstValid != -1) {
			break;
		}
	}
	if (*firstValid == -1) {
		//  All Missing!
		*firstValid = 0;
		*lastValid = 0;
		return 1;
	}
	//  Look for last non missing	
	//  (Has to be one, since we just checked for all miss)
	*lastValid = -1;
	for (i=(numberValues-1); i>=0; i--) {
		if (profileDepthsNumber == 0) {
			if (!zisMissingFloat(values[i])) {
				*lastValid = i;
				break;
			}
		}
		else {
			for (j=0; j<profileDepthsNumber; j++) {
				ipos = (i * profileDepthsNumber) + j;
				if (!zisMissingFloat(values[ipos])) {
					*lastValid = i;
					break;
				}
			}
			if (*lastValid != -1) 
				break;
		}
		if (sizeQuality > 0) {
			for (j=0; j<sizeQuality; j++) {
				ipos = (i * sizeQuality) + j;
				if (quality[ipos] != 0) {
					if (*lastValid == -1) {
						*lastValid = i;
					}
					break;
				}
			}
		}
		if (sizeNotes > 0) {
			for (j=0; j<sizeNotes; j++) {
				ipos = (i * sizeNotes) + j;
				if (notes[ipos] != 0) {
					if (*lastValid == -1) {
						*lastValid = i;
					}
					break;
				}
			}
		}
		if (cnotesLengthTotal > 0) {
			ipos = cnotesLengthTotal - (numberValues-i);
			if (ipos > 0) {				 
				if (cnotes[ipos] != '\0') {
					if (*lastValid == -1) {
						*lastValid = i;
					}
					break;
				}
			}
		}
		if (*lastValid != -1) {
			break;
		}
	}
	return 0;
}



int ztsFirstLastValidDouble(double *values, int profileDepthsNumber, int numberValues,
							 int *quality, int sizeQuality,
							 int *notes, int sizeNotes,
							 char *cnotes, int cnotesLengthTotal,
						     int *firstValid, int *lastValid)
{
	int i;
	int j;
	int ipos;

	//  Look for first non missing
	*firstValid = -1;
	for (i=0; i<numberValues; i++) {
		if (profileDepthsNumber == 0) {
			if (!zisMissingDouble(values[i])) {
				*firstValid = i;
				break;
			}
		}
		else {
			for (j=0; j<profileDepthsNumber; j++) {
				ipos = (i * profileDepthsNumber) + j;
				if (!zisMissingDouble(values[ipos])) {
					*firstValid = i;
					break;
				}
			}
			if (*firstValid != -1) 
				break;
		}
		if (sizeQuality > 0) {
			for (j=0; j<sizeQuality; j++) {
				ipos = (i * sizeQuality) + j;
				if (quality[ipos] != 0) {
					if (*firstValid == -1) {
						*firstValid = i;
					}
					break;
				}
			}
		}
		if (sizeNotes > 0) {
			for (j=0; j<sizeNotes; j++) {
				ipos = (i * sizeNotes) + j;
				if (notes[ipos] != 0) {
					if (*firstValid == -1) {
						*firstValid = i;
					}
					break;
				}
			}
		}
		if (cnotesLengthTotal > 0) {
			//  A missing or non-valid C note is '\0'.  Anything else is valid
			if (i < cnotesLengthTotal) {
				if (cnotes[i] != '\0') {
					if (*firstValid == -1) {
						*firstValid = i;
					}
					break;
				}
			}
		}
		if (*firstValid != -1) {
			break;
		}
	}
	if (*firstValid == -1) {
		//  All Missing!
		*firstValid = 0;
		*lastValid = 0;
		return 1;
	}

	//  Look for last non missing	
	//  (Has to be one, since we just checked for all miss)
	*lastValid = -1;
	for (i=(numberValues-1); i>=0; i--) {
		if (profileDepthsNumber == 0) {
			if (!zisMissingDouble(values[i])) {
				*lastValid = i;
				break;
			}
		}
		else {
			for (j=0; j<profileDepthsNumber; j++) {
				ipos = (i * profileDepthsNumber) + j;
				if (!zisMissingDouble(values[ipos])) {
					*lastValid = i;
					break;
				}
			}
			if (*lastValid != -1) 
				break;
		}
		if (sizeQuality > 0) {
			for (j=0; j<sizeQuality; j++) {
				ipos = (i * sizeQuality) + j;
				if (quality[ipos] != 0) {
					if (*lastValid == -1) {
						*lastValid = i;
					}
					break;
				}
			}
		}
		if (sizeNotes > 0) {
			for (j=0; j<sizeNotes; j++) {
				ipos = (i * sizeNotes) + j;
				if (notes[ipos] != 0) {
					if (*lastValid == -1) {
						*lastValid = i;
					}
					break;
				}
			}
		}
		if (cnotesLengthTotal > 0) {
			ipos = cnotesLengthTotal - (numberValues-i);
			if (ipos > 0) {				 
				if (cnotes[ipos] != '\0') {
					if (*lastValid == -1) {
						*lastValid = i + 1;
					}
					break;
				}
			}
		}
		if (*lastValid != -1) {
			break;
		}
	}
	return 0;
}
