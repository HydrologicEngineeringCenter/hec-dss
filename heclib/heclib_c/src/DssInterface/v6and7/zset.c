#include <string.h>

#include "heclib.h"
#include "hecdssInternal.h"


int zset(const char* parameter, const char* charVal, int integerValue)
{
	int  ival;
	int  status = 0;
	char cval[17];

	ival = integerValue;
	return zset7(parameter, charVal, integerValue);

/* ?
	cval[0] = '\0';
	zquery("VERS", cval, sizeof(cval), &ival);
	if (ival == 7) {
		status = zquery(parameter, cval, sizeof(cval), &ival);
	}

	return status;
	*/
}

int zset7_(const char* parameter, const char* charVal, int *integerValue, size_t lenParam, size_t lenCharVal)
{
	int rval =0;
	char *c_parm;
	char *c_char;
	c_parm = stringFortToC(parameter, lenParam);
	c_char = stringFortToC(charVal,lenCharVal);
	rval = zset7(c_parm, c_char, *integerValue);

	free(c_parm);
	free(c_char);
    return rval;

}
