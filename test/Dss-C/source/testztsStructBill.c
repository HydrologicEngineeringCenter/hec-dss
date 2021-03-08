#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "heclib.h"
#include "hecdss7.h"
#include "hecdssInternal.h"
#include "TestDssC.h"

	//  Regular interval floats, basic
int testReadInfo(long long *ifltab, const char *pathname);

int testztsStructBill()
{
	long long ifltab[500];
	long long ifltab2[500];
	zStructTimeSeries *tss1, *tss2;
	
	size_t len;
	int status;
	int numb;
	char pathname[100];
	char filename[100];
	char mess[500];
	zStructRecordSize *recordSize;
	int zero = 0;

	
	stringCopy(pathname, sizeof(pathname), "/EF RUSSIAN/COYOTE/STOR-RES EOP/01MAR2006/1HOUR//", sizeof(pathname));
	stringCopy(filename, sizeof(filename), "sample7.dss", sizeof(filename));
	len = strlen(filename);
	zopen_(ifltab2, filename, &status, len);

	recordSize = (zStructRecordSize *)zstructRecordSizeNew(pathname);
	status = zgetRecordSize(ifltab2, recordSize);

	tss1 = zstructTsNew(pathname); 
	status = ztsRetrieve(ifltab2, tss1, -1, 1, 0);
	
	status = zerrorCheck();
	zinquireChar(ifltab2, "ERROR", mess, sizeof(mess), &numb);
	testReadInfo(ifltab2, (const char *)pathname);
	zclose(ifltab2);


	stringCopy(pathname, sizeof(pathname), "//BELLE MEADE/FLOW/01MAR2014/15MIN/U0R0/", sizeof(pathname));
	stringCopy(filename, sizeof(filename), "s6.dss", sizeof(filename));
	len = strlen(filename);
	zopen_(ifltab, filename, &status, len);
	//zset("MLVL", "", 15);
	tss2 = zstructTsNew(pathname); 
	status = ztsRetrieve(ifltab, tss2, -1, 1, 0);

	stringCopy(pathname, sizeof(pathname), "//BELLE MEADE/FLOW/16MAR2014 - 20MAR2014/15MIN/U0R0/", sizeof(pathname));
	stringCopy(filename, sizeof(filename), "s7.dss", sizeof(filename));
	len = strlen(filename);
	zopen_(ifltab2, filename, &status, len);
	tss1 = zstructTsNew(pathname); 
	status = ztsRetrieve(ifltab2, tss1, -1, 1, 0);
	testReadInfo(ifltab2, (const char *)pathname);

	zstructFree(recordSize);
	zstructFree(tss2);
	if (status != STATUS_OKAY) return status;

	return 0; 
}

int testReadInfo(long long *ifltab, const char *pathname)
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


   

	zrinfo_ (ifltab, pathname, (int *)status, (int *)type,
			ctype, (int *)ldoubles, (int *)lquality, (int *)precision, 
			tag, lwdate, lwtime, progName, 
			(int *)version, (int *)numberData, (int *)spaceUsed, 
			(int *)compression, (int *)password, strlen (pathname),
			sizeof (ctype) -1, sizeof (tag) -1, sizeof (lwdate) -1, 
			sizeof (lwtime) -1, sizeof (progName) -1);

	return 0;
}

