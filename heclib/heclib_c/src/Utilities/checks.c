#include "checks.h"

void checkdoubles_(double* dataOrig, double* dataRead, int* number, char* mess, int* status, size_t dummy) {
	int i;

	*status = 0;

	for (i = 0; i < *number; i++) {
		if (dataOrig[i] != dataRead[i]) {
			printf("\n");
			printf("\n");
			printf("*****  Data read does not match those written  *****\n");
			printf("At ordinate: %8d Written: %12.3f  Read: %12.3f\n", i + 1, dataOrig[i], dataRead[i]);
			printf("%s\n", mess);
			printf("\n");
			printf("\n");
			*status = -1;
			return;
		}
	}

	return;
}


void checknumbers_(int* numberOrig, int* numberRead, const char* mess, int* status, size_t dummy) {
	if (*numberOrig != *numberRead) {
		printf("\n");
		printf("\n");
		printf("*****  Number read does not match that written  *\n");
		printf("Number Written: %10d  Read: %10d\n", *numberOrig, *numberRead);
		printf("%s\n", mess);
		printf("\n");
		printf("\n");
		*status = -1;
	}
	else {
		*status = 0;
	}
}

#include <stdio.h>
#include <stdlib.h>

void checkfloats_(float* dataOrig, float* dataRead, int* number, char* mess, int* status, size_t dummy) {
	int i;

	*status = 0;

	for (i = 0; i < number; i++) {
		if (dataOrig[i] != dataRead[i]) {
			printf("\n");
			printf("\n");
			printf("*****  Data read does not match those written  *****\n");
			printf("At ordinate: %8d Written: %12.3f Read: %12.3f\n", i + 1, dataOrig[i], dataRead[i]);
			printf("%s\n", mess);
			printf("\n");
			printf("\n");
			*status = -1;
			return;
		}
	}
}

void checkints_(int* dataOrig, int* dataRead, int* length, int* number, char* mess, int* status, size_t mess_len)
{
	int i, j;
	for (i = 0; i < *number; i++) {
		for (j = 0; j < *length; j++) {
			if (dataOrig[i * (*length) + j] != dataRead[i * (*length) + j]) {
				printf("\n\n");
				printf("*****  Data read does not match those written *****\n");
				printf("At ordinate: %8d Written: %12d Read: %12d\n",
					i + 1, dataOrig[i * (*length) + j], dataRead[i * (*length) + j]);
				printf("%.*s\n", mess_len, mess);
				printf("\n\n");
				*status = -1;
				return;
			}
		}
	}
	*status = 0;
}