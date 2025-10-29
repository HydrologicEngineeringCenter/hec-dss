#include "TestDssC.h"
#include <string.h>
/*
* Testing to support larger F part.
* 
* args:
*   dssFileName -  dss file to be created (existing file will be overwritten)
*/
int PathnameTesting(char* dssFileName)
{
	long long ifltab[250];
	zStructTimeSeries* tss1;
	double dvalues[200];
	int status, i;
	memset(ifltab,0,sizeof(ifltab));
	deleteFile(dssFileName);
	
	printf("\n reading %s\n",dssFileName);
	status = hec_dss_zopen(ifltab, dssFileName);
	if (status != STATUS_OKAY) return status;

	//  Write a regular interval data set.  Gen up the data
	for (i=0; i<200; i++) {
		dvalues[i] = (float)i;
	}
	const char* path = "//HELLS CANYON-DAM/FLOW-RES-OUT/01Nov2021/6HOUR/C:000001|T:20211127-1600|V:20211127-173000|ProfJudgeOSI|R:A0B0C0D0E0|MyFPartWhatever/";
	//const char* path = "//HELLS CANYON-DAM/FLOW-RES-OUT/01Nov2021/6HOUR/MyFPartWhatever/";
	char fpart[MAX_PART_SIZE];
	printf("path='%s'", path);
	printf("\nMAX_PATH_LENGTH %d", MAX_PATHNAME_LENGTH);
	printf("\nMAX_PART_SIZE %d", MAX_PART_SIZE);
	printf("\nPath length = %d", (int)strlen(path));
	zpathnameGetPart(path, 6, fpart, MAX_PART_SIZE);
	printf("\nFpart='%s'", fpart);
	printf("\nFpart Length =%d", (int)strlen(fpart));
	printf("\n");
	tss1 = zstructTsNewRegDoubles(path, dvalues, 200, "21Nov2021", "1200", "cfs", "Inst-Val");
	status = ztsStore(ifltab, tss1, 0);
	zstructFree(tss1);
	if (status != STATUS_OKAY) {
		zclose(ifltab);
		return status; 
	}

	
	zStructTimeSeries* tss2 = zstructTsNew(path);
	tss2->boolRetrieveAllTimes = 1;
	status = ztsRetrieve(ifltab, tss2, -1, 2, 0);

	if (status != STATUS_OKAY) {
		zstructFree(tss2);
		zclose(ifltab);
		return status;
		}

	if (tss2->numberValues != 200) {
		printf("\nError reading path '%s'", path);
		zstructFree(tss2);
		zclose(ifltab);
		return -1;
	  }

	char cdate[13], ctime[10];
	for (int i = 0; i < tss2->numberValues;  i++) {
		getDateAndTime(tss2->times[i], tss2->timeGranularitySeconds, tss2->julianBaseDate,
			cdate, sizeof(cdate), ctime, sizeof(ctime));
		printf("%s %s, %f\n", cdate, ctime, tss2->doubleValues[i]);
	}

	zstructFree(tss2);
	zclose(ifltab);
	return 0; 
}

