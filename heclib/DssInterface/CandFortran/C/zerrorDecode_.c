#include "heclib.h"
#include "hecdssInternal.h"

void zerrordecode_(int *errorCode, int *highFunction, int *lowFunction, int *dssError, int *status, int *severity)
{
	*severity = zerrorDecode(*errorCode, highFunction, lowFunction, dssError, status);
}

