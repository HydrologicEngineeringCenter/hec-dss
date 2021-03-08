#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
//#include <Windows.h>
//#include "tinydir.h"
//#include <dirent.h>
#include "heclib.h"


int findInFile()
{

	char name[256];
	char filename[256];
	long long longVal;
	int status;
	int ihandle;
	int i;
	long long buff[1000];
	long long iaddress;
	int numberFound;

	printf("Enter DSS file name: ");
	status = scanf("%s", filename);

	printf("\nEnter LONG value to search for: ");
	status = scanf("%lld", &longVal);


	status = zopenDisk(filename, &ihandle, 0, 0);
	if (status) {
		printf("Error opening file, status = %d\n", status);
	}

	iaddress = 0;
	numberFound = 0;
	while (!status) {
		status = zreadDisk(ihandle, 0, iaddress, buff, 200);
		if (!status) {
			for (i = 0; i < 100; i++) {
				if (buff[i] == longVal) {
					iaddress += (long long)i;
					printf("\nValue found at address %lld\n\n", iaddress);
					numberFound++;
					//_close(ihandle);
					//return 0;
				}
			}
		}
		else {
			printf("status = %d\n", status);
			break;
		}
		iaddress += 100L;
	}

	if (!numberFound) {
		printf("Value not found\n");		
	}
	else {
		printf("number found = %d\n", numberFound);
	}
	printf("Last address = %lld\n", iaddress);
	closeFile(ihandle);
	return 0;


}

