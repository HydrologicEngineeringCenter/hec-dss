#include <stdio.h>
#include <string.h>

#include "heclib.h"


int gridPreCompressionTest() {

	long long ifltab[250] = { 0 };
	int status = hec_dss_zopen(ifltab, "2017-06-28_event.dss");
	if (status != 0) {
		printf("Error during open.  status= %d\n", status);
		return status;
	}
	char* path = "/SHG1k/Iowa50km/PRECIPITATION/26JUN2017:2300/26JUN2017:2400/AORC/";
	char* path2 = "/SHG1k/Iowa50km/PRECIPITATION/26JUN2017:2300/26JUN2017:2400/AORC2/";
	if (status < 0) {
		printf("Error during catalog.  Error code %d\n", status);
		return status;
	}
	zStructSpatialGrid* gridStructRetrieve;
	zStructSpatialGrid* gridStructRetrieve2;

	gridStructRetrieve = zstructSpatialGridNew(path);
	status = zspatialGridRetrieve(ifltab, gridStructRetrieve, 1);

	void* gridData = gridStructRetrieve->_data;

	int dataSize = (gridStructRetrieve->_numberOfCellsX * gridStructRetrieve->_numberOfCellsY);
	int numLongs = ((dataSize - 1) / 2) + 1;
	int* dataValues = calloc(numLongs, 8);
	if (!dataValues) {
		printf("\nout of memory?");
		zstructFree(gridStructRetrieve);
		zclose(ifltab);
		return -1;
	}
	memcpy(dataValues, gridStructRetrieve->_data, dataSize * 4);
	void* old_data = gridStructRetrieve->_data;
	char* old_pathname = gridStructRetrieve->pathname;
	void* buffer;
	int bufsize = compress_zlib(dataValues, numLongs * 2 * 4, &buffer);

	gridStructRetrieve->_data = buffer;
	gridStructRetrieve->pathname = path2;

	zspatialGridStore_extended(ifltab, gridStructRetrieve, bufsize);

	gridStructRetrieve2 = zstructSpatialGridNew(path2);
	status = zspatialGridRetrieve(ifltab, gridStructRetrieve2, 1);

	gridStructRetrieve->pathname = old_pathname;
	free(dataValues);
	free(old_data);
	zstructFree(gridStructRetrieve2);
	zstructFree(gridStructRetrieve);
	if (status != STATUS_OKAY) {
		printf("Error retrieving grid: %d", status);
		return status;
	}

	zclose(ifltab);
	return 0;
}