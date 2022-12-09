#include <stdio.h>
#include <string.h>

#include "heclib.h"
#include "hecdssInternal.h"
#include "zStructTimeSeries.h"
#include "TestDssC.h"

int stringTests();
int miscTests()
{
	int status = stringTests();
	return status;
}
int stringTests() {

	char* units = "M    ";
	char* trimmedUnits = mallocAndCopyTrim(units);
	if (strlen(trimmedUnits) != 1) {
		free(trimmedUnits);
		return STATUS_NOT_OKAY;
	}
	
	if (trimmedUnits)
		free(trimmedUnits);
	
	char* units2 = "    "; // all blanks return \0
	trimmedUnits = mallocAndCopyTrim(units2);
	int len = strlen(trimmedUnits);
	if (len != 0) {
		free(trimmedUnits);
		return STATUS_NOT_OKAY;
	}
	if (trimmedUnits)
		free(trimmedUnits);



	char* units3 = "  M "; // blanks both sides 
	trimmedUnits = mallocAndCopyTrim(units3);
	len = strlen(trimmedUnits);
	if (len != 1) {
		free(trimmedUnits);
		return STATUS_NOT_OKAY;
	}
	if (trimmedUnits)
		free(trimmedUnits);

	return STATUS_OKAY;
}

