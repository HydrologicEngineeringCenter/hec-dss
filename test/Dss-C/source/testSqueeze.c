#include "stdio.h"
#include "string.h"

#include "heclib.h"
#include "hecdss7.h"
#include "hecdssInternal.h"
#include "TestDssC.h"



int testSqueeze(const char *filename)
{
	int DEBUG=0;
	int status;

	status = zsqueeze(filename);
	if (status != 0) {
		printf ("** Error during squeeze, status = %d\n", status);
	}
	else {
		printf("Squeeze passed.\n");
	}
	
	return status;
}