// GridTest.cpp : Defines the entry point for the console application.
//

#include "stdio.h"
#include "string.h"
#include "math.h"
#include "zdssKeys.h"
#include "heclib.h"
#include "hecdss7.h"
#include "hecdssInternal.h"
#include "zStructAllocation.h"

#include "zStructSpatialGrid.h"

int isSame(float f1, float f2) {
	if (fabsf(f1 - f2) < 1.0e-6)
		return 1;
	else
		return 0;
}

int readGrid(long long *ifltab, char *path) {
	zStructSpatialGrid *gridStructRetrieve;
	int status = 0;
	float *data;
	int idx;

	gridStructRetrieve = zstructSpatialGridNew(path);
	status = zspatialGridRetrieve(ifltab, gridStructRetrieve, 1);

	if (status != STATUS_OKAY) {
		printf("Error retrieving grid: %d", status);
		return status;
	}

	printGridStruct(ifltab, 0, gridStructRetrieve);
	data = (float *)gridStructRetrieve->_data;
	int dataSize = gridStructRetrieve->_numberOfCellsX * gridStructRetrieve->_numberOfCellsY;
	for (idx = 0; idx < dataSize; idx++)
		if (!isSame(data[idx], idx * 1.2))
			printf("******** Data is different at: %d : %f ********\n", idx, data[idx]);
	data = (float *)gridStructRetrieve->_rangeLimitTable;
	for (idx = 0; idx < gridStructRetrieve->_numberOfRanges; idx++) {
		if (!isSame(data[idx], idx * 1.1))
			printf("******** Range is different at: %d : %f ********\n", idx, data[idx]);
		if (gridStructRetrieve->_numberEqualOrExceedingRangeLimit[idx] != idx * 2)
			printf("******** Histo is different at: %d : %f ********\n", idx, data[idx]);
	}
	float *min = gridStructRetrieve->_minDataValue, *max = gridStructRetrieve->_maxDataValue, *mean = gridStructRetrieve->_meanDataValue;
	if (!isSame(*min, 0.001))
		printf("******** Min is different at: %f ********\n", *min);
	if (!isSame(*max, (dataSize)*1.2))
		printf("******** Max is different at: %f:%f ********\n", *max, dataSize * 1.2);
	if (!isSame(*mean, (*max) / 2.0))
		printf("******** Mean is different at: %f:%f ********\n", *mean, (*max) / 2.0);
	zstructFree(gridStructRetrieve);
	return status;
}

int writeGrid(long long *ifltab, char *path, int rows, int cols, int cm, int range) {
	zStructSpatialGrid *gridStructStore;
	float *data;
	int idx;
	int status = 0;

	static float *rangelimit;
	static int *histo;

	rangelimit = calloc(range, sizeof(float));
	histo = calloc(range, sizeof(float));

	for (idx = 0; idx < range; idx++) {
		rangelimit[idx] = idx * 1.1;
		histo[idx] = idx * 2;
	}
	if (zgetVersion(ifltab) != 7) {
		return 0;
	}

	gridStructStore = zstructSpatialGridNew(path);

	//   Gen data
	gridStructStore->_type = 420;
	gridStructStore->_dataSource = mallocAndCopy("INTERNAL");
	gridStructStore->_version = 1;
	gridStructStore->_dataUnits = mallocAndCopy("mm");
	gridStructStore->_dataType = PER_AVER;
	gridStructStore->_lowerLeftCellX = 0;
	gridStructStore->_lowerLeftCellY = 0;
	gridStructStore->_numberOfCellsX = cols;
	gridStructStore->_numberOfCellsY = rows;
	gridStructStore->_cellSize = 5.0;
	gridStructStore->_compressionMethod = cm;

	gridStructStore->_rangeLimitTable = &(rangelimit[0]);
	gridStructStore->_numberEqualOrExceedingRangeLimit = &(histo[0]);
	gridStructStore->_numberOfRanges = range;
	gridStructStore->_srsDefinitionType = 1;
	gridStructStore->_srsName = mallocAndCopy("SRC_NAME");
	gridStructStore->_srsDefinition = mallocAndCopy("TEST");
	gridStructStore->_xCoordOfGridCellZero = 10.2;
	gridStructStore->_yCoordOfGridCellZero = 20.3;
	gridStructStore->_nullValue = 99999.999;
	gridStructStore->_timeZoneID = mallocAndCopy("PST");
	gridStructStore->_timeZoneRawOffset = 8;
	gridStructStore->_isInterval = 1;
	gridStructStore->_isTimeStamped = 0;

    float* max = malloc(sizeof(float));
	*max = (rows * cols)*1.2;
	float* min = malloc(sizeof(float));
	 *min = 0.001;
	 float* mean = malloc(sizeof(float));
	 *mean = (rows * cols * 1.2) / 2.0;
	gridStructStore->_maxDataValue =  max;
	gridStructStore->_minDataValue = min;
	gridStructStore->_meanDataValue = mean;

	// Data
	data = (float*)calloc(gridStructStore->_numberOfCellsX * gridStructStore->_numberOfCellsY, sizeof(float));

	if (data != NULL) {
		for (idx = 0; idx < gridStructStore->_numberOfCellsX * gridStructStore->_numberOfCellsY; idx++)
			data[idx] = (idx * 1.2);
		gridStructStore->_data = data;

		status = zspatialGridStore(ifltab, gridStructStore);

		if (status != STATUS_OKAY) {
			printf("Error storing grid: %d", status);
		}

		//printGridStruct(ifltab, 0, gridStructStore);
		//free(data);
		zstructFree(gridStructStore);

	}
	return status;
}

void printUsage(char *progname) {
	fprintf(stderr, "Usage: %s -d dss_file_name -p path -h height -w width -c compression_method (26 for zlib and 1 for uncompressed)  -r range -v\n",
		progname);
	exit(EXIT_FAILURE);
}
int getopt(int nargc, char * const nargv[], const char *ostr);
extern char *optarg;

int main(int argc, char *argv[]) {
	int status, opt, rows = 0, cols = 0, cm = -1, verify = 0, range = 0;
	long long ifltab7[250];
	char *dssFile = NULL;
	char *path = "/a/b/c/01jan2001:1200/01jan2001:1300/f/";
	while ((opt = getopt(argc, argv, "d:p:h:w:c:r:v")) != -1) {
		switch (opt) {
		case 'd':
			dssFile = optarg;
			break;
		case 'p':
			path = optarg;
			break;
		case 'h':
			rows = atoi(optarg);
			break;
		case 'w':
			cols = atoi(optarg);
			break;
		case 'c':
			cm = atoi(optarg);
			break;
		case 'r':
			range = atoi(optarg);
			break;
		case 'v':
			verify = 1;
			break;
		default: /* '?' */
			printUsage(argv[0]);
		}
	}
	zset("mlvl", "", 1);
	if (dssFile == NULL)
		printUsage(argv[0]);
	if (!verify)
		remove(dssFile);
	memset(ifltab7, 0, 250 * sizeof(long long));
	status = zopen(ifltab7, dssFile);
	if (status != STATUS_OKAY) {
		printf("Error opeing file: %d", status);
		return -1;
	}
	if (!verify) {
		if (rows == 0 || cols == 0 || cm == -1 || range == 0)
		   {
			printUsage(argv[0]);
			return -1;
		   }
		if(writeGrid(ifltab7, path, rows, cols, cm, range))
		return -1;

	}
	//if( readGrid(ifltab7, path))
	//return -1;
	if( zclose(ifltab7))
	return -1;
   return 0;
}

