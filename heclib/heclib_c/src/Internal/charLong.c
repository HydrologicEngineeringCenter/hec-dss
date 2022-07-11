#include "hecdss7.h"
//#  Note - No header included for def, as to avoid compiler errors!

/**
*  Function:	charLong
*
*  Description:	Convert a character string to a long array, or a long array to a character string
*				for storage as int*8 within the DSS file (all writes and reads in DSS are in integer longs.)
*				This function accomidated endian differences, which can make this function complex
*

*  Declaration: int charLong(void *from, void *to, int numberBytes, int maxBytesTo, int boolToLong, int zeroEndFlag);
*
*  Parameters:	void *from
*					Either a character string or a long array, that will be copied from (you decide).
*
*				void *to
*					Either a character string or a long array, that will be copied too.
*
*				int numberBytes
*					The number of bytes to copy.
*					If 0 (zero), copy until null terminator is found.
*
*				int maxBytesTo
*					Length of *to.  No more than this number of bytes will be copied
*
*				int boolToLong
*					If *to is a long, set this to 1.  If *to is a character string, set to 0 (zero).
*					How and where endian byte swapping occurs is dependent on this flag.
*
*				int zeroEndFlag
*					A flag indicating if the unused bytes at the end of "to" should be zeroed out.
*					Set to -1 not to zero out or null terminate
*					Set to 0 to null terminate
*					Set to 1 to zero out the last word.
*					Set to 2 to zero out the end up to maxBytesTo.
*
*	Returns:	The number of long words (8 byte) that were converted
*
*	Note:		THIS FUNCTION IS REQUIRED TO BE USED WHEN MOVING CHARACTERS IN / OUT OF ifltab
*					OR ANY OTHER INT OR LONG ARRAY!!!
*					This makes the code transportable across platforms
*					Machine specific function, based on big endian / little endian.
*
*	See Also;	charInt()  (Last int word is zeroed out)
*
*
*	Author:			Bill Charley
*	Date:			2012
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int charLong(void *from, void *to, int numberBytes, int maxBytesTo, int boolToLong, int zeroEndFlag)
{
	int i;
	int j;
	int len;
	int ipos;
	int done;
	int nlongwords;
	int numbWordsToZero;
	unsigned char *charFrom;
	unsigned char *charTo;
	unsigned long long *longArray;
	unsigned long long imLong;


	charFrom = (unsigned char *)from;
	charTo = (unsigned char *)to;

	nlongwords = ((numberBytes - 1) / 8) + 1;
	
	if (!bigEndian()) {
		if (numberBytes > 0) {
			if (numberBytes > maxBytesTo) numberBytes = maxBytesTo;
			ipos = 0;
			for (i = 0; i < numberBytes; i++) {
				charTo[ipos++] = charFrom[i];
			}
			if (!boolToLong && (zeroEndFlag >= 0) && (ipos < maxBytesTo)) charTo[ipos] = '\0';
		}
		else {
			numberBytes = 0;
			for (i = 0; i < maxBytesTo; i++) {
				charTo[numberBytes++] = charFrom[i];
				if (charFrom[i] == '\0') break;
			}
		}
		if (zeroEndFlag > 0) {
			if (zeroEndFlag == 1) {
				numbWordsToZero = nlongwords;
			}
			else {
				numbWordsToZero = ((maxBytesTo - 1) / 8) + 1;
			}
			len = numbWordsToZero * 8;
			if (len > maxBytesTo) len = maxBytesTo;
			if (numberBytes < len) {
				for (i = numberBytes; i < len; i++) {
					charTo[i] = '\0';
				}
			}
		}
	}
	else {
		if (boolToLong) {
			//  From a character string to a long long array
			//  Swap long long array after copying 
			if (numberBytes > 0) {
				if (numberBytes > maxBytesTo) numberBytes = maxBytesTo;
				ipos = 0;
				for (i = 0; i < numberBytes; i++) {
					charTo[ipos++] = charFrom[i];
				}
			}
			else {
				numberBytes = 0;
				for (i = 0; i < maxBytesTo; i++) {
					charTo[numberBytes++] = charFrom[i];
					if (charFrom[i] == '\0') break;
				}
			}
			if (zeroEndFlag > 0) {
				if (zeroEndFlag == 1) {
					numbWordsToZero = nlongwords;
				}
				else {
					numbWordsToZero = ((maxBytesTo - 1) / 8) + 1;
				}
				len = numbWordsToZero * 8;
				if (len > maxBytesTo) len = maxBytesTo;
				if (numberBytes < len) {
					for (i = numberBytes; i < len; i++) {
						charTo[i] = '\0';
					}
				}
			}
			longArray = (unsigned long long*)to;
			nlongwords = ((maxBytesTo - 1) / 8) + 1;
			zswap(longArray, 2 * nlongwords);			
		}
		else {
			//  From a long long array to a character string
			//  On big endian machines, we need to swap the long long array, then copy to characters
			longArray = (unsigned long long*)from;
			if (numberBytes > 0) {
				if (numberBytes > maxBytesTo) numberBytes = maxBytesTo;
				ipos = 0;
				for (j = 0; j < nlongwords; j++) {
					//  Have to copy to ensure original from is not touched.
					imLong = longArray[j];
					zswap((long long *)&imLong, 2);
					charFrom = (unsigned char *)&imLong;
					for (i = 0; i < 8; i++) {
						charTo[ipos++] = charFrom[i];
						if (ipos >= numberBytes) break;
					}					
				}
				if ((zeroEndFlag >= 0) && (ipos < maxBytesTo)) charTo[ipos] = '\0';
			}
			else {
				numberBytes = 0;
				nlongwords = ((maxBytesTo - 1) / 8) + 1;
				done = 0;
				for (j = 0; j < nlongwords; j++) {
					imLong = longArray[j];
					zswap((long long *)&imLong, 2);
					charFrom = (unsigned char *)&imLong;
					for (i = 0; i < 8; i++) {
						charTo[numberBytes++] = charFrom[i];
						if (charFrom[i] == '\0') done = 1;
						if (numberBytes >= maxBytesTo) done = 1;
						if (done) break;
					}
					if (done) break;
				}
			}
			if (zeroEndFlag > 0) {
				if (zeroEndFlag == 1) {
					numbWordsToZero = nlongwords;
				}
				else {
					numbWordsToZero = ((maxBytesTo - 1) / 8) + 1;
				}
				len = numbWordsToZero * 8;
				if (len > maxBytesTo) len = maxBytesTo;
				if (numberBytes < len) {
					for (i = numberBytes; i < len; i++) {
						charTo[i] = '\0';
					}
				}
			}
		}		
	}

	return nlongwords;
}
