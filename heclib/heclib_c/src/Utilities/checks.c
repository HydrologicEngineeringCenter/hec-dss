#include "checks.h"

void checkdoubles_(double* dataOrig, double* dataRead, int* number, const char* mess, int* status, size_t dummy) {
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

void checkfloats_(float* dataOrig, float* dataRead, int* number, const char* mess, int* status, size_t dummy) {
	int i;

	*status = 0;

	for (i = 0; i < *number; i++) {
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

void checkints_(int* dataOrig, int* dataRead, int* length, int* number, const char* mess, int* status, size_t mess_len)
{
	int i, j;
	for (i = 0; i < *number; i++) {
		for (j = 0; j < *length; j++) {
			if (dataOrig[i * (*length) + j] != dataRead[i * (*length) + j]) {
				printf("\n\n");
				printf("*****  Data read does not match those written *****\n");
				printf("At ordinate: %8d Written: %12d Read: %12d\n",
					i + 1, dataOrig[i * (*length) + j], dataRead[i * (*length) + j]);
				printf("%s\n", mess);
				printf("\n\n");
				*status = -1;
				return;
			}
		}
	}
	*status = 0;
}


void upcase(char* str) {
	for (int i = 0; str[i]; i++) {
		str[i] = toupper((unsigned char)str[i]);
	}
}

void checkstring_(char* stringOrig, char* stringRead, char* mess, int* status,
	size_t stringOrigLen, size_t stringReadLen, size_t messLen) {
	int max_len = 2001;
	char strOrig[2001], stringR[2001];
	
	stringOrig[sizeof stringOrig - 1] = 0;
	stringRead[sizeof stringRead - 1] = 0;

	if (strlen(stringOrig) >= max_len || strlen(stringRead) >= max_len) {
		*status = -1;
		printf("\nError in checkstring_. Input char* was too long %s ", mess);
		return;
	}

	if (strncmp(stringOrig, stringRead, stringOrigLen) != 0) {
		// Allow different case strings
		strOrig[sizeof strOrig - 1] = 0;
		stringR[sizeof stringR - 1] = 0;

		strncpy(strOrig, stringOrig, sizeof(strOrig) - 1);
		strncpy(stringR, stringRead, sizeof(stringR) - 1);

		if (stringR[sizeof stringR - 1] != 0
			|| strOrig[sizeof stringR - 1] != 0) {

			printf("\nError in checkstring_. input char* was too long %s ", mess);
			return;
		}

		strOrig[sizeof(strOrig) - 1] = 0;
		stringR[sizeof(stringR) - 1] = 0;

		upcase(strOrig);
		upcase(stringR);

		if (strcmp(strOrig, stringR) != 0) {
			printf("\n\n");
			printf("***  String read does not match that written *****\n");
			printf("String Written: ==>%s<==  Read: ==>%s<==\n", stringOrig, stringRead);
			printf("%s\n", mess);
			printf("\n\n");
			*status = -1;
			return;
		}
	}

	*status = 0;
}
