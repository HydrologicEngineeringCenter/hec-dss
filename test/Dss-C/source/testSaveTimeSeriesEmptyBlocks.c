#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

#include "heclib.h"
#include "hecdss7.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "TestDssC.h"


/// <summary>
/// in DSSVue editing the units does not work. 
/// empty blocks don't appear to save units and datatype strings.
/// </summary>
/// <returns></returns>
int saveWithEmptyBlocks()
{
	long long ifltab[250] = { 0 };
	int status = hec_dss_zopen(ifltab, "CantSaveEdits4.dss");
	if (status != 0) {
		printf("Error during open.  status= %d\n", status);
		return status;
	}
  const char* pathname = "/Bardwell Lk/Ennis, TX/Maximum Lake or reservoir water surface elevation above NGVD 192//1Day/USGS/";
	zStructTimeSeries* tss = zstructTsNew(pathname);
	int flagTrimData = -1;
  ztsRetrieve(ifltab, tss, flagTrimData, 1, 0);

	printf("\nunits are %s\n", tss->units == NULL ? "null" : tss->units);
	char cdate[13], ctime[10];

	for (int i = 0; i < tss->numberValues; i++) {
		getDateAndTime(tss->times[i], tss->timeGranularitySeconds, tss->julianBaseDate,
			cdate, sizeof(cdate), ctime, sizeof(ctime));
		printf("\n%s %s, %f", cdate, ctime, tss->floatValues[i]);
		if (i > 10) {
			printf("\n....\n");
			break;
		}
	}

	status = tss->units != NULL ? 0 : -1;
	if (tss->units == NULL) {
		printf("\nUnits are null, expected 'ft'\n");
	}
	zstructFree(tss);
  
	zclose(ifltab);

	return status;

}