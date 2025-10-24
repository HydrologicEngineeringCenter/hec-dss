#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "hecdss7.h"
#include "hecdssInternal.h"
#include "TestDssC.h"

int readRecordInfo(long long* ifltab, const char* pathname);

int testRecordInfo6(const char *filename) {
	long long ifltab2[500];
	zStructTimeSeries* tss1, * tss2;

	size_t len;
	int status;
	int numb;
	char pathname[100];
	char mess[500];
	zStructRecordSize* recordSize;
	int zero = 0;


	stringCopy(pathname, sizeof(pathname), "/A/B/C/01JAN2019/1DAY/F/", sizeof(pathname));
	len = strlen(filename);
	status = hec_dss_zopen(ifltab2, filename);

	recordSize = (zStructRecordSize*)zstructRecordSizeNew(pathname);
	status = zgetRecordSize(ifltab2, recordSize);

	tss1 = zstructTsNew(pathname);
	status = ztsRetrieve(ifltab2, tss1, -1, 1, 0);

	readRecordInfo(ifltab2, (const char*)pathname);
	zclose(ifltab2);

	return 0;
}

int testRecordInfo7(const char* filename) {
	long long ifltab2[500];
	zStructTimeSeries* tss1, * tss2;

	size_t len;
	int status;
	int numb;
	char pathname[100];
	char mess[500];
	zStructRecordSize* recordSize;
	int zero = 0;


	stringCopy(pathname, sizeof(pathname), "/a/b/c/01Jan2020/1Day/f/", sizeof(pathname));
	len = strlen(filename);
	status = hec_dss_zopen(ifltab2, filename);

	recordSize = (zStructRecordSize*)zstructRecordSizeNew(pathname);
	status = zgetRecordSize(ifltab2, recordSize);

	tss1 = zstructTsNew(pathname);
	status = ztsRetrieve(ifltab2, tss1, -1, 1, 0);

	readRecordInfo(ifltab2, (const char*)pathname);
	zclose(ifltab2);

	return 0;
}

int readRecordInfo(long long* ifltab, const char* pathname)
{

	int  status[1];
	int  type[1];
	int  ldoubles[1];
	int  lquality[1];
	int  precision[1];
	int  version[1];
	int  numberData[1];
	int  spaceUsed[1];
	int  compression[1];
	int  password[4];


	char ctype[51];

	char tag[9];
	char lwdate[13];
	char lwtime[13];
	char progName[9];




	zrinfo_(ifltab, pathname, (int*)status, (int*)type,
		ctype, (int*)ldoubles, (int*)lquality, (int*)precision,
		tag, lwdate, lwtime, progName,
		(int*)version, (int*)numberData, (int*)spaceUsed,
		(int*)compression, (int*)password, strlen(pathname),
		sizeof(ctype) - 1, sizeof(tag) - 1, sizeof(lwdate) - 1,
		sizeof(lwtime) - 1, sizeof(progName) - 1);

	return 0;
}