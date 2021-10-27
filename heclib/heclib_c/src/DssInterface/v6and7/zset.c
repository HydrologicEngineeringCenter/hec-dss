#include <string.h>

#include "heclib.h"
#include "hecdssInternal.h"

int zset(const char* parameter, const char* charVal, int integerValue)
{
	int ival;

	ival = integerValue;
	zset_(parameter, charVal, &ival, strlen(parameter), strlen(charVal));

	//zset6_(parameter, charVal, &ival, strlen(parameter), strlen(charVal));
	//zset7(parameter, charVal, integerValue);

	return 0;
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



