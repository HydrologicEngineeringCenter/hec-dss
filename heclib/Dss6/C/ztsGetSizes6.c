#include <string.h>

#include "hecdssInternal.h"
#include "heclib.h"
#include "heclib6.h"


//  Get TS Record Sizes
//  Gets size information about a record or data set (series of records)
//  Time Series may be either regular or irregular


//  The purpose of this function is to know how to dimension your arrays
//  for reading a record

//  Reports sizes for blocks, not within blocks

//
int ztsGetSizes6(long long *ifltab, zStructTimeSeries *tss, zStructRecordSize *recordSize)
{

	int status;
	int boolIrreg;
	int julianBlockDate;
	int nhead, ndata, lfound;
	int npath;
	int idtype;
	char path[MAX_PATHNAME_LENGTH];
	char fortran_pathname[MAX_PATHNAME_LENGTH];
	char blockDate[20];
	char cdtype[30];

	//  Initialize all to zero, so we can add for multiple records
	//  (or just return if pathname not found)

	recordSize->dataType = 0;
	recordSize->version = 0;
	recordSize->numberValues = 0;
	recordSize->logicalNumberValues = 0;
	recordSize->values1Number = 0;
	recordSize->values2Number = 0;
	recordSize->values3Number = 0;
	recordSize->internalHeaderNumber = 0;
	recordSize->header2Number = 0;
	recordSize->userHeaderNumber = 0;
	recordSize->allocatedSize = 0;
	recordSize->lastWriteTimeMillis = 0;
	recordSize->numberRecordsFound = 0;
	recordSize->tsPrecision = 0;
	recordSize->tsTimeOffset = 0;
	recordSize->tsProfileDepthsNumber = 0;
	recordSize->tsBlockStartPosition = 0;
	recordSize->tsBlockEndPosition = 0;
	recordSize->tsValueSize = 0;
	recordSize->tsValueElementSize = 0;
	recordSize->tsValuesCompressionFlag = 0;
	recordSize->tsQualityElementSize = 0;
	recordSize->tsQualityCompressionFlag = 0;
	recordSize->tsInotesElementSize = 0;
	recordSize->tsInotesCompressionFlag = 0;
	recordSize->tsCnotesLength = 0;

	if (!tss->timeWindow) {
		boolIrreg = ztsProcessTimes(ifltab, tss, 0);
		if (zmessageLevel(ifltab, MESS_METHOD_TS_READ_ID, MESS_LEVEL_USER_DIAG)) {
			ztsMessTimeWindow(ifltab, 0, tss);
		}
	}
	else {
		if (tss->timeWindow->intervalSeconds > 0) {
			boolIrreg = 0;
		}
		else {
			boolIrreg = 1;
		}
	}

	if (boolIrreg) {
		status = STATUS_RECORD_NOT_FOUND;
		stringCopy(path, sizeof(path), tss->pathname, strlen(tss->pathname));
		julianBlockDate = tss->timeWindow->startBlockJulian;
		while (julianBlockDate <= tss->timeWindow->endBlockJulian) {

			//  Get the path for this block and compute next block date
			julianToDate(julianBlockDate, 4, blockDate, sizeof(blockDate));
			zpathnameSetPart (path, sizeof(path), blockDate, 4);
			julianBlockDate = ztsIncrementBlock(julianBlockDate, tss->timeWindow->blockSize);

			
   		    stringCToFort(fortran_pathname, sizeof(fortran_pathname),  path);

			//  Read info and header
			npath = (int)strlen(path);
			zcheck6_(ifltab, fortran_pathname, &npath, &nhead, &ndata, &lfound, strlen(path));

			if (lfound) {
				status = STATUS_RECORD_FOUND;
				recordSize->numberRecordsFound++;
				//  number returned from zcheck includes times
				recordSize->numberValues += ndata/2;
				recordSize->userHeaderNumber = nhead;
				//  Get the data type, if our first one
				if (recordSize->dataType == 0) {
					zdtype_(ifltab, path, &ndata, &lfound, cdtype, &idtype, strlen(path), sizeof(cdtype));
					recordSize->dataType = idtype;
				}
			}
			else {
				//  Not found.
			}
		}
	}
	else {
		//  Regular Interval data
		recordSize->numberValues = tss->numberValues;
		status = STATUS_OKAY;
	}


	return status;
}


