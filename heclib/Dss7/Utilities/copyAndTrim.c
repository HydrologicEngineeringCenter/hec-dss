#include <string.h>

#include "heclib7.h"
#include "hecdssInternal.h"

//  copies, setting first last non blank to char(0)
int copyAndTrim(char *tostring, size_t maxToLen, const char *fromString, size_t fromStringLen)
{
	int len;
	int maxToL;
	int fromLen;

	len = (int)strlen(fromString);
	maxToL = (int) maxToLen;
	fromLen = (int) fromStringLen;
	if (len < fromLen) {
		fromLen = len;
	}
	len = (int)trimLengthLen(fromString, (size_t)fromLen);
	if (len >= maxToL) {
		len = maxToL;
	}
	stringCopy(tostring, (size_t)maxToL, fromString, (size_t)len);
	return len;
}

