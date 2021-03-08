#include <stdio.h>
#include <string.h>
#include <io.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <share.h>

#include "heclib.h"
 

int main(int argc, char* argv[])
{
	long long ifltab[250];
	char filename[_MAX_PATH];
	char pathname[393];
	char *tempname;
	zStructCatalog *catStruct;
	int exists;
	int idummy;
	int loop;
	int numberPaths;
	int numberBytes;
	int ihandle;
	int status, i, j;
	long lbyte;

	if (argc != 2)
	{
		printf("\nUsage: CatalogTest file.dss\n");
		return -1;
	}
		exists = zfileName(filename, sizeof(filename), argv[1], &idummy);
		if (!exists) {
			printf("*** Error - DSS file must exist for this sample.  File: %s\n", filename);
			return -1;
		}
		status = zopen(ifltab, filename);
		if (status != STATUS_OKAY) return status;

		//  Get a regular full sorted catalog and print first 5 pathnames
		catStruct = zstructCatalogNew();
		//int zcatalog(long long *ifltab, char *pathWithWild, zStructCatalog *catStruct, int boolSorted);
		numberPaths = zcatalog(ifltab, (const char*)0, catStruct, 1);
		if (numberPaths < 0) return numberPaths;
		printf("%d total pathnames found\n", catStruct->numberPathnames);
		if (numberPaths > 5) numberPaths = 5;
		for (i = 0; i < numberPaths; i++) printf("%s\n", catStruct->pathnameList[i]);
		zstructFree(catStruct);

		//  Now get all pathnames that have "Flow" in the C part
		catStruct = zstructCatalogNew();
		numberPaths = zcatalog(ifltab, "/*/*/*Flow*/*/*/*/", catStruct, 1);
		if (numberPaths < 0) return numberPaths;
		printf("%d pathnames with Flow found\n", catStruct->numberPathnames);
		if (numberPaths > 5) numberPaths = 5;
		for (i = 0; i < numberPaths; i++) printf("%s\n", catStruct->pathnameList[i]);
		zstructFree(catStruct);

		//  Get all paired data pathnames - new feature in DSS-7, not DSS-6
		if (zgetVersion(ifltab) == 7) {
			catStruct = zstructCatalogNew();
			catStruct->typeWantedStart = DATA_TYPE_PD;  //  Paired data regular
			catStruct->typeWantedEnd = DATA_TYPE_PDD;   //  Paired data double
			numberPaths = zcatalog(ifltab, (const char*)0, catStruct, 1);
			if (numberPaths < 0) return numberPaths;
			printf("%d paired data pathnames found\n", catStruct->numberPathnames);
			if (numberPaths > 5) numberPaths = 5;
			for (i = 0; i < numberPaths; i++) printf("%s\n", catStruct->pathnameList[i]);
			zstructFree(catStruct);
		}

		//  Create a new catalog (only pathname list) using the standard catalog name (.dsc)
		//  int zcatalogFile(long long *ifltab, const char *catalogFilename, int boolSorted, const char *pathWithWildChars);
		numberPaths = zcatalogFile(ifltab, (const char *)0, 1, (const char *)0);
		if (numberPaths < 0) return numberPaths;
		printf("Regular catalog created with %d pathnames.\n", numberPaths);

		//  Use our own file name
		strncat_s(filename, sizeof(filename), "cat.txt", _TRUNCATE);
		numberPaths = zcatalogFile(ifltab, filename, 1, (const char *)0);
		if (numberPaths < 0) return numberPaths;
		printf("Regular catalog written to file %s.\n", filename);

		//  Write to a temporary file under our control.  Note, uses a handle, not a file pointer
		tempname = _tempnam("", "cat_unsorted.txt");
		if (!tempname) return -1;
		//status = _sopen_s (&ihandle, tempname, (_O_CREAT | _O_TEXT), _SH_DENYNO, (_S_IREAD | _S_IWRITE));
		status = _sopen_s(&ihandle, tempname, (_O_CREAT | O_RDWR | _O_TEXT), _SH_DENYNO, 0);
		if (status) return status;
		// int zcatalogToFile(long long *ifltab, int catalogHandle, int fortranUnit, int boolSorted)
		numberPaths = zcatalogToFile(ifltab, ihandle, 0, 0);
		if (numberPaths < 0) return numberPaths;
		printf("Unsorted list written to temporary file %s.\n", tempname);
		if (numberPaths > 5) numberPaths = 5;
		lbyte = 0;
		for (i = 0; i < numberPaths; i++) {
			//  Low level read line.  (Would normally use getline, if not already opened)
			_lseek(ihandle, lbyte, 0);
			numberBytes = _read(ihandle, pathname, sizeof(pathname));
			if (numberBytes < 0) return numberBytes;
			for (j = 0; j < numberBytes; j++) {
				if (pathname[j] == '\n') {
					lbyte += j;
					lbyte += 2;  //  Get past \n and to start of next path
					pathname[j] = '\0';
					break;
				}
			}
			printf("%s\n", pathname);
		
		_close(ihandle);
		//  _unlink(filename);
		free(tempname);

		zclose(ifltab);
	}

	return 0;
}
