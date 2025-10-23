//#include <Time.h>
//#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include "string.h"
//#include <Winsock2.h>
//#include <process.h>
//#include <tchar.h>
//#include <Psapi.h>
#include <math.h>

#include "zdssMessages.h"
#include "hecdss7.h"
#include "heclib.h"
#include "hecdssInternal.h"
#include "TestDssC.h"


#include "hecdss7.h"
//#  Note - No header included for def, as to avoid compiler errors!


int charLongxx(void *from, void *to, int numberBytes, int maxBytesTo, int zeroEndFlag)
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
	unsigned long long *fromLong;
	unsigned long long imLong;


	charFrom = (unsigned char *)from;
	charTo = (unsigned char *)to;

	nlongwords = ((numberBytes - 1) / 8) + 1;
	if (!bigEndian()) {
		if (numberBytes > 0) {
			if (numberBytes > maxBytesTo) numberBytes = maxBytesTo;
			for (i = 0; i < numberBytes; i++) {
				charTo[i] = charFrom[i];
			}
		}
		else {
			numberBytes = 0;
			for (i = 0; i < maxBytesTo; i++) {
				charTo[numberBytes++] = charFrom[i];
				if (charFrom[i] == '\0') break;
			}
		}
	}
	else {

		//  On big endian machines, we need to swap characters
		//  This function is always used with 8 byte words, so we can swap safely
		fromLong = (unsigned long long*)from;
		if (numberBytes > 0) {
			if (numberBytes > maxBytesTo) numberBytes = maxBytesTo;
			ipos = 0;
			for (j = 0; j < nlongwords; j++) {
				imLong = fromLong[j];
				zswap(&imLong, 2);
				charFrom = (unsigned char *)&imLong;
				for (i = 0; i < 8; i++) {
					charTo[ipos++] = charFrom[i];
					if (ipos >= numberBytes) break;
				}
			}
		}
		else {
			numberBytes = 0;
			nlongwords = ((maxBytesTo - 1) / 8) + 1;
			done = 0;
			for (j = 0; j < nlongwords; j++) {
				imLong = fromLong[j];
				zswap(&imLong, 2);
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
	}

	if (zeroEndFlag) {
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

	return nlongwords;
}


char getCharacter(int i) {
	int j;


	j = i/26;
	i = i - (j * 26);

	i += 97;
	if ((i >= 97) && (i <= 122)) {
		return i;
	}
	return 97;
}

void pathStr(char *str, int pos) {
	int i, j;

	str[0] = '/';
	i = pos /22;
	j = pos - (i * 22);
	if (j < 0) j = -j;
	if (j < 5) j += 5;
	for (i=0; i<j; i++) {
		str[i+1] = getCharacter(pos+i);
	}
	str[j+1] = '\0';
}

void tstCnotes() {

	long long ifltab7[250];
	char fileName7[80];
	char cnotes[10000];
	int status;
	int  j, ipos;
	float values[200];
	zStructTimeSeries  *tss2;
	char alpha[] = { "abcdefghijklmnopqrstuvwxyz" };

	stringCopy(fileName7, sizeof(fileName7), "C:/Temp/char7.dss", sizeof(fileName7));
	status = hec_dss_zopen(ifltab7, fileName7);

	/*
	zStructTimeSeries* zstructTsNewRegDoubles(const char* pathname, double *doubleValues, int numberValues,
	const char *startDate, const char *startTime,
	const char *units, const char *type)
	*/

	//  Use "quality7[][]" for multiple int quality flags.
	//  Cannot use both quality and quality7, as they both occupy the same space on disk. 
	//int quality7[][] = new int[200][4];
	//String cnotes[] = new String[200];
	ipos = 0;
	for (int i = 0; i < 200; i++) {
		values[i] = (float)i;
		int n = i / 25;
		n = i - (n * 25) + 1;
		for (j = 0; j < n; j++) {
			cnotes[ipos++] = alpha[j];
		}
		cnotes[ipos++] = '\0';
		//cnotes[i] = alpha.substring(0, n);
	//	for (int j = 0; j<4; j++) {
	//		quality7[i][j] = (i * 10) + j;
	//	}
	}
	zset("mlvl", "", 14);
	tss2 = zstructTsNewRegFloats("/Basin/Location/Flow//1Hour/Quality and Notes/", values, 200, "20Jan2010", "2400", "cfs", "inst-val");
	tss2->cnotes = cnotes;
	tss2->cnotesLengthTotal = ipos;
	status = ztsStore(ifltab7, tss2, 1);

}