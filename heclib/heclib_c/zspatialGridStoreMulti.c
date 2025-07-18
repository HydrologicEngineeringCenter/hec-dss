#include <stdlib.h>
#include <string.h>

//#define PTW32_STATIC_LIB
#define HAVE_STRUCT_TIMESPEC  
#include <pthread.h>

#include "heclib.h"
#include "hecdssInternal.h"
#include "zdssKeys.h"

#include "zStructSpatialGrid.h"
#include "zspatialGridStoreMulti.h"


typedef struct
{
    long long *ifltab;    
    zStructSpatialGrid* grid;      

    zStructTransfer* xfer; 
    int status;  
} CompressTask;

#if defined(_WIN32) || defined(_WIN64)          /* ---- Windows ---- */
#include <windows.h>

static int cpuCount(void)
{
	SYSTEM_INFO si;
	GetSystemInfo(&si);
	return (int)si.dwNumberOfProcessors ? (int)si.dwNumberOfProcessors : 1;
}

#else                                           /* ---- POSIX ------- */
#include <unistd.h>

static int cpuCount(void)
{
	long n = sysconf(_SC_NPROCESSORS_ONLN);
	return (n > 0) ? (int)n : 1;
}
#endif


static int
compressOnly(long long* ifltab, zStructSpatialGrid* gridStruct,
    zStructTransfer** retXfer)  
{

    int status = STATUS_OKAY;
    zStructTransfer* ztransfer = NULL;

    /* <<<<<<<<<<<<<<<<<<<<<< ORIGINAL CODE HERE >>>>>>>>>>>>>>>>>>>>> */
    /* (Everything in zspatialGridStore up to but not including        */
    /*  "status = zwrite( ifltab, ztransfer );" )                      */

	int status;
	size_t count;
	size_t len;
	size_t total;
	char* str;
	int julianFirstValue, secondsFirstValue;
	int julianLastValue, secondsLastValue;
	int dateStatus;
	char dPart[MAX_PART_SIZE];
	char ePart[MAX_PART_SIZE];
	zStructTransfer* ztransfer;
	void* buffer;
	int bufsize;
	int* internalHeader;

	if (!gridStruct) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID, zdssErrorCodes.NULL_ARGUMENT,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "gridStruct is null");
	}
	if (!gridStruct->pathname) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID, zdssErrorCodes.NULL_PATHNAME,
			0, 0, zdssErrorSeverity.INVALID_ARGUMENT, "", "gridStruct pathname is null");
	}

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessage(ifltab, " ");
		zmessageDebug(ifltab, DSS_FUNCTION_zspatialGridStore_ID, "Enter; Pathname: ", gridStruct->pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_zspatialGridStore_ID, "Handle:  ", zhandle(ifltab));
		zmessageDebugInt(ifltab, DSS_FUNCTION_zspatialGridStore_ID, "Number of points:  ", gridStruct->_numberOfCellsX * gridStruct->_numberOfCellsY);
	}

	//  Check for correct DSS Version
	if (zgetVersion(ifltab) != 7) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID, zdssErrorCodes.INCOMPATIBLE_VERSION,
			zgetVersion((void*)ifltab), 0, zdssErrorSeverity.WARNING, "", "");
	}


	ztransfer = zstructTransferNew(gridStruct->pathname, 0);
	if (!ztransfer) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID,
			zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, 0, 0,
			zdssErrorSeverity.MEMORY_ERROR,
			gridStruct->pathname, "Allocating ztransfer struct");
	}
	ztransfer->dataType = gridStruct->_type;

	//  Header 1, basic info
	ztransfer->internalHeaderNumber = INT_HEAD_grid_internalHeaderNumber;


	ztransfer->internalHeaderMode = 1;
	internalHeader = (int*)calloc(ztransfer->internalHeaderNumber, 4);
	internalHeader[INT_HEAD_grid_structVersion] = gridStruct->_structVersion;
	internalHeader[INT_HEAD_grid_type] = gridStruct->_type;
	internalHeader[INT_HEAD_grid_version] = gridStruct->_version;
	internalHeader[INT_HEAD_grid_dataType] = gridStruct->_dataType;
	internalHeader[INT_HEAD_grid_lowerLeftCellX] = gridStruct->_lowerLeftCellX;
	internalHeader[INT_HEAD_grid_lowerLeftCellY] = gridStruct->_lowerLeftCellY;
	internalHeader[INT_HEAD_grid_numberOfCellsX] = gridStruct->_numberOfCellsX;
	internalHeader[INT_HEAD_grid_numberOfCellsY] = gridStruct->_numberOfCellsY;
	internalHeader[INT_HEAD_grid_compressionMethod] = gridStruct->_compressionMethod;
	internalHeader[INT_HEAD_grid_numberOfRanges] = gridStruct->_numberOfRanges;
	internalHeader[INT_HEAD_grid_srsDefinitionType] = gridStruct->_srsDefinitionType;
	internalHeader[INT_HEAD_grid_timeZoneRawOffset] = gridStruct->_timeZoneRawOffset;
	internalHeader[INT_HEAD_grid_isInterval] = gridStruct->_isInterval;
	internalHeader[INT_HEAD_grid_isTimeStamped] = gridStruct->_isTimeStamped;
	internalHeader[INT_HEAD_grid_storageDataType] = gridStruct->_storageDataType;



	//  Header 2, character information
	//  Count the number of characters (including nulls) in struct strings
	total = 0;
	if (gridStruct->_dataUnits) {
		len = strlen(gridStruct->_dataUnits);
		total += len;
	}
	total++; //  null terminator
	if (gridStruct->_dataSource) {
		len = strlen(gridStruct->_dataSource);
		total += len;
	}
	total++;
	if (gridStruct->_srsName) {
		len = strlen(gridStruct->_srsName);
		total += len;
	}
	total++;
	if (gridStruct->_srsDefinition) {
		len = strlen(gridStruct->_srsDefinition);
		total += len;
	}
	total++;
	if (gridStruct->_timeZoneID) {
		len = strlen(gridStruct->_timeZoneID);
		total += len;
	}
	total++;


	//  Copy strings into one array
	str = (char*)calloc(total, 1);
	count = 0;
	if (gridStruct->_dataUnits) {
		len = strlen(gridStruct->_dataUnits);
		stringCopy(&str[count], total, gridStruct->_dataUnits, len);
		count += len;
	}
	str[count++] = '\0';
	if (gridStruct->_dataSource) {
		len = strlen(gridStruct->_dataSource);
		stringCopy(&str[count], total, gridStruct->_dataSource, len);
		count += len;
	}
	str[count++] = '\0';
	if (gridStruct->_srsName) {
		len = strlen(gridStruct->_srsName);
		stringCopy(&str[count], total, gridStruct->_srsName, len);
		count += len;
	}
	str[count++] = '\0';
	if (gridStruct->_srsDefinition) {
		len = strlen(gridStruct->_srsDefinition);
		stringCopy(&str[count], total, gridStruct->_srsDefinition, len);
		count += len;
	}
	str[count++] = '\0';
	if (gridStruct->_timeZoneID) {
		len = strlen(gridStruct->_timeZoneID);
		stringCopy(&str[count], total, gridStruct->_timeZoneID, len);
		count += len;
	}
	str[count++] = '\0';


	ztransfer->header2Number = numberIntsInBytes((int)total);
	ztransfer->header2 = (int*)calloc(ztransfer->header2Number + 2, 4);
	ztransfer->header2Mode = 1;
	//  Need to be sure to copy char into in array, not cast and assign to header
	charInt(str, ztransfer->header2, (int)total, (ztransfer->header2Number * 4), 1, 1, 0);
	free(str);

	//  Store floats: _cellSize,_xCoordOfGridCellZero,_yCoordOfGridCellZero,_nullValue
	ztransfer->userHeaderNumber = (4 * 2) + 3;
	ztransfer->userHeader = (int*)calloc(ztransfer->userHeaderNumber, 4);
	ztransfer->userHeaderMode = 1;
	memset(ztransfer->userHeader, 0, ztransfer->userHeaderNumber * 4);
	memcpy(&ztransfer->userHeader[0], &gridStruct->_cellSize, 4);
	memcpy(&ztransfer->userHeader[1], &gridStruct->_xCoordOfGridCellZero, 4);
	memcpy(&ztransfer->userHeader[2], &gridStruct->_yCoordOfGridCellZero, 4);
	memcpy(&ztransfer->userHeader[3], &gridStruct->_nullValue, 4);
	if (bigEndian()) {
		zswitchInts(ztransfer->userHeader, ztransfer->userHeaderNumber);
	}

	//  Store additional compression parameters (for future use)
	ztransfer->values2Number = 0;
	ztransfer->values2 = 0;

	if (gridStruct->_storageDataType == GRID_FLOAT) {
		//  Range Limits and min/max/mean values
		ztransfer->values1Number = gridStruct->_numberOfRanges * 2 + 3;
		ztransfer->values1 = (int*)calloc(ztransfer->values1Number, 4);
		ztransfer->values1Mode = 1;
		convertDataArray((int*)(gridStruct->_minDataValue), &ztransfer->values1[0], 1, 1, 1);
		convertDataArray((int*)(gridStruct->_maxDataValue), &ztransfer->values1[1], 1, 1, 1);
		convertDataArray((int*)(gridStruct->_meanDataValue), &ztransfer->values1[2], 1, 1, 1);
		convertDataArray((int*)(gridStruct->_rangeLimitTable), &ztransfer->values1[3], gridStruct->_numberOfRanges, 1, 1);
		convertDataArray((int*)(&gridStruct->_numberEqualOrExceedingRangeLimit[0]), &ztransfer->values1[gridStruct->_numberOfRanges + 3], gridStruct->_numberOfRanges, 1, 1);
		int dataSize = (gridStruct->_numberOfCellsX * gridStruct->_numberOfCellsY);
		int numLongs = ((dataSize - 1) / 2) + 1;
		int* dataValues = calloc(numLongs, 8);
		memcpy(dataValues, gridStruct->_data, dataSize * 4);
		switch (gridStruct->_compressionMethod) {
		case NO_COMPRESSION:
			gridStruct->_sizeofCompressedElements = dataSize * 4;
			internalHeader[INT_HEAD_grid_sizeofCompressedElements] = gridStruct->_sizeofCompressedElements;
			ztransfer->values3Number = numLongs * 2;
			ztransfer->values3 = (int*)dataValues;
			if (bigEndian()) {
				zswitchInts(ztransfer->values3, ztransfer->values3Number);
			}


			break;
		case ZLIB_COMPRESSION:
#if 0
		{
			printf("XXX: Before Values: ");
			for (int i = 0; i < numLongs * 2; i++)
				printf("%x,", dataValues[i]);
			printf("\n");
		}
#endif
		if (bigEndian()) {
			zswap((long long*)dataValues, numLongs * 2);
			zswitchInts(dataValues, numLongs * 2);
		}



		bufsize = compress_zlib(dataValues, numLongs * 2 * 4, &buffer);
		/*if (compress_zfp(gridStruct->data, gridStruct->_numberOfCellsX, gridStruct->_numberOfCellsY, (float)1.0e-6, &buffer, &bufsize, 0)) */
		if (bufsize <= 0) {
			free(dataValues);
			zstructFree(ztransfer);
			return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID, zdssErrorCodes.WRITE_ERROR,
				0, 0, zdssErrorSeverity.WRITE_ERROR, "", "gridStruct error in zlib compression");
		}
#if 0
		{
			char* cdata = buffer;
			printf("XXX: Compressed Buffer: ");
			for (int i = 0; i < bufsize; i++)
				printf("%x,", (int)cdata[i]);
			printf("\n");
			printf("XXX: Values: ");
			for (int i = 0; i < numLongs * 2; i++)
				printf("%x,", dataValues[i]);
			printf("\n");
		}
#endif
		gridStruct->_sizeofCompressedElements = bufsize;
		internalHeader[INT_HEAD_grid_sizeofCompressedElements] = gridStruct->_sizeofCompressedElements;
		ztransfer->values3Number = numberIntsInBytes(bufsize);
		ztransfer->values3 = (int*)calloc((size_t)ztransfer->values3Number + 2, 4);
		charInt((void*)buffer, (void*)ztransfer->values3, bufsize, (ztransfer->values3Number * 4), 1, 1, 0);
		free(buffer);
		free(dataValues);
		break;
		default:
			zstructFree(ztransfer);
			return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID, zdssErrorCodes.WRITE_ERROR,
				0, 0, zdssErrorSeverity.WRITE_ERROR, "", "gridStruct: Unsupported Compression");

		}
		if (bigEndian()) {
			zswitchInts(ztransfer->values1, ztransfer->values1Number);
		}
	}
	else {
		zstructFree(ztransfer);
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID, zdssErrorCodes.WRITE_ERROR,
			0, 0, zdssErrorSeverity.WRITE_ERROR, "", "gridStruct: Unsupported storage data type");
	}



	//  Save the date!
	len = zpathnameGetPart(ztransfer->pathname, 4, dPart, sizeof(dPart));
	if (len > 12) {
		dateStatus = spatialDateTime(dPart, &julianFirstValue, &secondsFirstValue);
		if (dateStatus == STATUS_OKAY) {
			len = zpathnameGetPart(ztransfer->pathname, 5, ePart, sizeof(ePart));
			if (len < 5) {
				//  An empty E part indicating an instantaneous dataset.
				//  Set end to start
				julianLastValue = julianFirstValue;
				secondsLastValue = secondsFirstValue;
			}
			else {
				dateStatus = spatialDateTime(ePart, &julianLastValue, &secondsLastValue);
			}
			if (dateStatus == STATUS_OKAY) {
				ifltab[zdssKeys.kdataFirstDate] = i4toi8(julianFirstValue, secondsFirstValue);
				ifltab[zdssKeys.kdataLastDate] = i4toi8(julianLastValue, secondsLastValue);
			}
		}
	}
	ztransfer->internalHeader = internalHeader;
	if (bigEndian()) {
		zswitchInts(ztransfer->internalHeader, ztransfer->internalHeaderNumber);
	}

	if (zmessageLevel(ifltab, MESS_METHOD_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		printGridStruct(ifltab, DSS_FUNCTION_zspatialGridStore_ID, gridStruct);
	}

    /* On success give the caller ownership of the transfer struct   */
    *retXfer = ztransfer;
    return STATUS_OKAY;

FAIL:
	if (ztransfer) {
		zstructFree(ztransfer);
	}

    *retXfer = NULL;
    return status;
}


/*--------------------------------------------------------------------
 *  A very small `pthread` worker wrapper.
 *-------------------------------------------------------------------*/
static void*
compressThread(void* arg)
{
    CompressTask* t = (CompressTask*)arg;

    t->status = compressOnly(t->ifltab, t->grid, &t->xfer);
    /* Do *NOT* call zwrite here.  Simply return.                    */
    return NULL;
}


/*--------------------------------------------------------------------
 *  Public function
 *-------------------------------------------------------------------*/
int
zspatialGridStoreMulti(long long* ifltab,
	zStructSpatialGrid** grids,
	int                  gridCount,
	int                  maxThreads)
{
	if (gridCount <= 0 || !grids) {
		return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID, zdssErrorCodes.INVALID_NUMBER, gridCount, 0, zdssErrorSeverity.WARNING, "", "gridCount <= 0");
	}

	/* -------------------------------------------------------------
	 *  Step 1 – fire up compression threads
	 * ------------------------------------------------------------*/
	int nCores = cpuCount();
	if (maxThreads <= 0 || maxThreads > nCores) {
		maxThreads = nCores;
	}
	if (maxThreads > gridCount) {
		maxThreads = gridCount;
	}

    pthread_t* threads = calloc((size_t)gridCount, sizeof(pthread_t));
    CompressTask* tasks = calloc((size_t)gridCount, sizeof(CompressTask));

    int nextToLaunch = 0;
    int liveThreads = 0;

    /* Very light-weight manual thread pool so we don’t pull in an
     * external dependency.                                         */
    while (nextToLaunch < gridCount || liveThreads) {
        /* Launch up to maxThreads concurrently              */
        while (liveThreads < maxThreads && nextToLaunch < gridCount) {
            CompressTask* t = &tasks[nextToLaunch];

            t->ifltab = ifltab;
            t->grid = grids[nextToLaunch];
            t->xfer = NULL;
            t->status = STATUS_OKAY;

            if (pthread_create(&threads[nextToLaunch], NULL, compressThread, t) != 0) {
                /* Could not create thread – fail fast */
                free(threads);
                free(tasks);
                return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID, zdssErrorCodes.WRITE_ERROR, 0, 0, zdssErrorSeverity.WRITE_ERROR, "", "pthread_create failed");
            }
            ++nextToLaunch;
            ++liveThreads;
        }

        /* Wait for the *oldest* running thread to finish so we can
         * start another one if necessary.                           */
        int doneIdx = nextToLaunch - liveThreads;

        if (pthread_join(threads[doneIdx], NULL) != 0)
        {
            free(threads);
            free(tasks);
            return zerrorProcessing(ifltab, DSS_FUNCTION_zspatialGridStore_ID, zdssErrorCodes.WRITE_ERROR, 0, 0, zdssErrorSeverity.WRITE_ERROR, "", "pthread_join failed");
        }
        --liveThreads;
    }

    /* -------------------------------------------------------------
     *  Step 2 – serial write to the DSS file
     * ------------------------------------------------------------*/
    for (int i = 0; i < gridCount; ++i)
    {
        int status = tasks[i].status;

        if (status == STATUS_OKAY)
        {
            /* Write the prepared transfer struct           */
            status = zwrite(ifltab, tasks[i].xfer);
        }

        /* Regardless of success or failure we must free   */
		if (tasks[i].xfer) {
			zstructFree(tasks[i].xfer);
		}

        if (zisError(status))
        {
            /* Clean-up remaining compressed transfers      */
			for (int j = i + 1; j < gridCount; ++j) {
				if (tasks[j].xfer) {
					zstructFree(tasks[j].xfer);
				}
			}

            free(threads);
            free(tasks);

            /* Propagate the first error code upward        */
            return zerrorUpdate(ifltab, status, DSS_FUNCTION_zspatialGridStore_ID);
        }
    }

    free(threads);
    free(tasks);
    return STATUS_OKAY;
}