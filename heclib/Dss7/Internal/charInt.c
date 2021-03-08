#include <string.h>

#include "hecdssInternal.h"


/**
*
*  *****************  DEPRECIATED   ***********************
*	Use charLong instead
*	Need to keep character strings on 8 byte boundaris (64-bit words) and 
*	not allow mixed types (e.g., and int and char) within an 8 byte word
*	because this is error prone for big endian machines.
*	
*  Function:	charInt
*
*  Description:	Convert a character string to an int array, or an int array to a
*				character string for storage as the DSS file.  
*				Function charLong is recommended over this, if applicable.
*
*  Declaration: int charInt(void *fromBuff, void *toBuff, int numberBytes, int maxBytesTo, int boolToInt, int zeroEnd, int boolMiddleLong);
*
*  Parameters:	void *fromBuff
*					Either a character string or a int array, that will be copied from (you decide).
*
*				void *toBuff
*					Either a character string or a int array, that will be copied to.
*					For Big endian machines, this must have a length for long long words,
*					i.e., not int to[1] or int to[3], but int to[2] or int to[4], etc.
*					On Big Endian, char strings must be long enough for int 8 words.
*
*				int numberBytes
*					The number of bytes to copy.
*					If 0 (zero), copy until null terminator is found.
*
*				int maxBytesTo
*					Don't copy more than this number of bytes!
*
*				int boolToInt
*					If *to is an int (not a char), set this to 1.  If *to is a character string, set to 0 (zero).
*					How and where endian byte swapping occurs is dependent on this flag.
*
*				int zeroEnd
*					A flag indicating if the unused bytes in the last word should be zeroed out.
*					Set to 1 to zero out the last word, or zero not to.
*
*				int boolMiddleLong
*					Not used on a little endian machine (PC).
*					If on a big endian machine, and you are starting in the middle of a long
*					then set this to 1 to have the correct offset.  e.g., for "abcdefgh", int ival[10]...
*					Little endian, ival[0], ival = "abcdefgh"
*					Big endian, ival[0], ival = "hgfedcba", swapped ="abcdefgh"
*					Little endian, ival[1], ival = "....abcd"
*					Big endian, boolMiddleLong=0, ival[1], ival = "....hgfe", swapped ="efgh...."
*					Big endian, boolMiddleLong=1, ival[1], ival = "dcba....", swapped ="....abcd" (== little endian)
*
*	Returns:	The number of int words (4 byte) that were converted
*
*	Note:		Machine specific function, based on big endian / little endian.
*
*	See Also;	charLong()  (Last long word is zeroed out)
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/
int charInt(void *fromBuff, void *toBuff, int numberBytes, int maxBytesTo, int boolToInt, int zeroEnd, int boolMiddleLong)
{
	int i;
	int j;
	int ipos;
	int jpos;
	int len;
	int nwords;
	int numberLongs;
	
	unsigned char c1[8];
	unsigned char c2[8];
	unsigned char *charFrom = (unsigned char *)fromBuff;
	unsigned char *charTo = (unsigned char *)toBuff;
	unsigned char *charToStart = (unsigned char *)toBuff;
	int *from = (int *)fromBuff;
	int *to = (int *)toBuff;

	if (numberBytes <= 0) {
		numberBytes = (int)strlen((const char *)charFrom);
	}

	if (maxBytesTo < numberBytes) {
		numberBytes = maxBytesTo;
	}
	nwords = numberIntsInBytes(numberBytes);
	if (!getEndian()) {
		for (i = 0; i < numberBytes; i++) {
			charTo[i] = charFrom[i];
		}
		if (zeroEnd) {
			len = nwords * 4;
			if (numberBytes < len) {
				for (i = numberBytes; i < len; i++) {
					charTo[i] = '\0';
				}
			}
		}
	}
	else {
		numberLongs = numberLongsInBytes(numberBytes);
		if (!boolMiddleLong) {
			ipos = 0;
			if (boolToInt) {
				for (i = 0; i < numberLongs; i++) {
					for (j = 7; j >= 0; j--) {
						jpos = (i * 8) + j;
						charTo[jpos] = charFrom[ipos++];
						if (ipos >= numberBytes) break;
					}
				}
			}
			else {
				for (i = 0; i < numberLongs; i++) {
					for (j = 7; j >= 0; j--) {
						jpos = (i * 8) + j;
						charTo[ipos++] = charFrom[jpos];
						if (ipos >= numberBytes) break;
					}
				}
			}
		}
		else {
			//  Starts in the middle of a long word - much more complicated!
			//  Need to start 1 int before, and end 1 int after
			charToStart -= 4;
			convertDataType((void *)charToStart, (void *)&c1[0], 1, 1);
			convertDataType(&from[0], (void *)&c1[4], 1, 1);
			if (numberBytes < 4) {
				for (j = numberBytes; j < 4; j++) c1[j + 4] = '\0';
			}
			*(unsigned char *)&c2[0] = *(unsigned char *)&c1[7];
			*(unsigned char *)&c2[1] = *(unsigned char *)&c1[6];
			*(unsigned char *)&c2[2] = *(unsigned char *)&c1[5];
			*(unsigned char *)&c2[3] = *(unsigned char *)&c1[4];
			*(unsigned char *)&c2[4] = *(unsigned char *)&c1[0];
			*(unsigned char *)&c2[5] = *(unsigned char *)&c1[1];
			*(unsigned char *)&c2[6] = *(unsigned char *)&c1[2];
			*(unsigned char *)&c2[7] = *(unsigned char *)&c1[3];
			convertDataType((void *)c2, (void *)charToStart, 2, 2);

			if (nwords > 2) {
				for (i = 1; i < numberLongs; i++) {
					ipos = (i * 2) - 1;
					convertDataType(&from[ipos], (void *)c1, 2, 2);
					jpos = numberBytes - (ipos * 4);
					if (jpos < 8) {
						for (j = jpos; j < 8; j++) c1[j] = '\0';
					}
					*(unsigned char *)c2 = *(unsigned char *)&c1[7];
					*(unsigned char *)&c2[1] = *(unsigned char *)&c1[6];
					*(unsigned char *)&c2[2] = *(unsigned char *)&c1[5];
					*(unsigned char *)&c2[3] = *(unsigned char *)&c1[4];
					*(unsigned char *)&c2[4] = *(unsigned char *)&c1[3];
					*(unsigned char *)&c2[5] = *(unsigned char *)&c1[2];
					*(unsigned char *)&c2[6] = *(unsigned char *)&c1[1];
					*(unsigned char *)&c2[7] = *(unsigned char *)c1;
					convertDataType((void *)c2, &to[(i * 2) - 1], 2, 2);
				}
			}

			if ((nwords > 1) && !isOdd(nwords)) {
				convertDataType(&from[nwords - 1], (void *)&c1[0], 1, 1);
				jpos = numberBytes - ((nwords - 1) * 4);
				if (jpos < 4) {
					for (j = jpos; j < 4; j++) c1[j] = '\0';
				}
				charToStart = (unsigned char *)to + numberBytes;
				convertDataType((void *)charToStart, (void *)&c1[4], 1, 1);
				*(unsigned char *)&c2[0] = *(unsigned char *)&c1[7];
				*(unsigned char *)&c2[1] = *(unsigned char *)&c1[6];
				*(unsigned char *)&c2[2] = *(unsigned char *)&c1[5];
				*(unsigned char *)&c2[3] = *(unsigned char *)&c1[4];
				*(unsigned char *)&c2[4] = *(unsigned char *)&c1[3];
				*(unsigned char *)&c2[5] = *(unsigned char *)&c1[2];
				*(unsigned char *)&c2[6] = *(unsigned char *)&c1[1];
				*(unsigned char *)&c2[7] = *(unsigned char *)&c1[0];
				convertDataType((void *)c2, (void *)&to[nwords - 1], 2, 2);
			}
		}
	}

	return nwords;
}
