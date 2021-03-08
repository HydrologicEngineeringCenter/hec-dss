#include <stdio.h>
#include <string.h>

#include "heclib.h"
#include "hecdssInternal.h"
#include "zStructTimeSeries.h"
#include "TestDssC.h"


int runTests(long long* ifltab)
{

	int status;
	
	status = testInternalIO(ifltab);
	if (status != STATUS_OKAY) return status;

	status = testWriteRead(ifltab);
	if (status != STATUS_OKAY) return status;

	status = testArrayWriteRead(ifltab);
	if (status != STATUS_OKAY) return status;

	status = testBufferedIO(ifltab);
	if (status != STATUS_OKAY) return status;

	status = testZtsProcessTimes(ifltab);
	if (status != STATUS_OKAY) return status;
			
	status = testlocation(ifltab);
	if (status != STATUS_OKAY) return status;

	status = testztsStruct1(ifltab);
	if (status != STATUS_OKAY) return status;

	status = testztsStruct2(ifltab);
	if (status != STATUS_OKAY) return status;

	status = testztsStruct3(ifltab);
	if (status != STATUS_OKAY) return status;

	status = testztsStruct4(ifltab);
	if (status != STATUS_OKAY) return status;

	status = testztsStruct11(ifltab);
	if (status != STATUS_OKAY) return status;

	status = testztsStruct12(ifltab);
	if (status != STATUS_OKAY) return status;

	//  Simple test to write tand then overwrite same data set
	status = testztsStruct12(ifltab);
	if (status != STATUS_OKAY) return status;
	
	status = testztsStruct13(ifltab);
	if (status != STATUS_OKAY) return status;
	
	status = testztsStruct14(ifltab);
	if (status != STATUS_OKAY) return status;
	
	status = testztsStruct15(ifltab);
	if (status != STATUS_OKAY) return status;
	
	status = testztsStruct5(ifltab);
	if (status != STATUS_OKAY) return status;
	
	status = testTimeSeriesPattern(ifltab);
	if (status != STATUS_OKAY) return status;
		
	status = testExpandedTimes(ifltab);
	if (status != STATUS_OKAY) return status;
	
	status = testExpandedTimesIrreg(ifltab);
	if (status != STATUS_OKAY) return status;
	
	
	status = testExpandedTimesIrreg2(ifltab);
	if (status != STATUS_OKAY) return status;
	
	
/////////   FIX ME NOW!!!!!!!!!!!!!!!!!
	/////  NEED TO WRITE A TEST TO TEST TRIM INCLUDING CNOTES AND QUALITY!!!!
	//////////////////////////////////////////////////////////////////////

	status = testTextTable(ifltab);
	if (status != STATUS_OKAY) return status;

	status = testProfileReg(ifltab);
	if (status != STATUS_OKAY) return status;		

	status = testProfileIrreg(ifltab);
	if (status != STATUS_OKAY) return status;
	
	status = testPairedData(ifltab);
	if (status != STATUS_OKAY) return status;

	status = TestPairedData2(ifltab);
	if (status != STATUS_OKAY) return status;
	
	status = TestPairedData3(ifltab);
	if (status != STATUS_OKAY) return status;
	
	
	status = testTin(ifltab);
	if (status != STATUS_OKAY) return status;

	status = testAlias(ifltab);
	if (status) return status;

	status = zcheckFile(ifltab);
	if (status != STATUS_OKAY) {
		printf("Failure in zcheckFile!\n");
		return status;
	}
	
	zclose(ifltab);
	

	zmessage(ifltab, "\n\nCompleted run test sequence.\n\n");

	return status;
}


