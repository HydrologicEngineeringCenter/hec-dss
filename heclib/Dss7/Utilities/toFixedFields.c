#include <string.h>

#include "hecdssInternal.h"


int toFixedFields(char *cstring, size_t sizeofCstring, char **fields, int *fieldLengths, int nfields,
				  int *columnStart, int *boolLeftJustify)
{
	int i;
	int j;
	int count;
	int len;
	int diff;
	int nblanks;
	char *cfield;

	count = 0;
	if(columnStart[0] > 0) {
		for (i=0; i<columnStart[0]; i++) {
			cstring[count++] = ' ';
			if ((count-1) == (int)sizeofCstring) {
				cstring[count-1] = '\0';
				return count;
			}
		}
	}

	for (j=0; j<nfields; j++) {
		if (count < columnStart[j]) {
			diff = columnStart[j] - count;
			for (i=0; i<diff; i++) {
				cstring[count++] = ' ';
				if ((count-1) == (int)sizeofCstring) {
					cstring[count-1] = '\0';
					return count;
				}
			}
		}

		cfield = fields[j];
		len = fieldLengths[j];
		if (j < (nfields-1)) {
			diff = columnStart[j+1] - columnStart[j];
		}
		else {
			diff = len;
		}
		if (len > diff) {
			len = diff;
		}
		if (!boolLeftJustify[j]) {
			nblanks = diff - len;
			for (i=0; i<nblanks; i++) {
				cstring[count++] = ' ';
				if ((count-1) == (int)sizeofCstring) {
					cstring[count-1] = '\0';
					return count;
				}
			}
		}
		for (i=0; i<len; i++) {
			cstring[count++] = cfield[i];
			if ((count-1) == (int)sizeofCstring) {
				cstring[count-1] = '\0';
				return count;
			}
		}
	}
	cstring[count++] = '\0';
	return count;
}

